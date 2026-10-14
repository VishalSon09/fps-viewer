{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Main (main) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (object, (.=))
import           Data.List (isSuffixOf, nub)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Environment (getArgs, lookupEnv)
import           Text.Read (readMaybe)
import           Yesod.Core

import           Library

data App = App
  { appMatches  :: [MatchEntry]
  , appByMatch  :: M.Map Text [MatchEntry]   -- match id -> one entry per player
  , appMinimaps :: M.Map Text FilePath
  }

mkYesod "App" [parseRoutes|
/                HomeR    GET
/api/index       IndexR   GET
/api/game/#Text  GameR    GET
/minimap/#Text   MinimapR GET
|]

instance Yesod App

-- | Maps (with coordinate config) + every match file, for the filter dropdowns.
getIndexR :: Handler Value
getIndexR = do
  app <- getYesod
  let maps = [ object [ "id" .= mcId c, "scale" .= mcScale c, "ox" .= mcOx c, "oz" .= mcOz c
                      , "image" .= ("/minimap/" <> mcId c) ]
             | c <- mapConfigs, mcId c `M.member` appMinimaps app ]
  returnJson (object ["maps" .= maps, "matches" .= appMatches app])

-- | All players' events for one match id. Parsed from the Parquet files on demand.
getGameR :: Text -> Handler Value
getGameR matchId = do
  app <- getYesod
  case M.lookup matchId (appByMatch app) of
    Nothing -> notFound
    Just es -> do
      loaded <- liftIO (mapM (\e -> (,) e <$> loadEvents e) es)
      returnJson [ object ["id" .= meId e, "player" .= mePlayer e, "events" .= evs]
                 | (e, Right evs) <- loaded ]

getMinimapR :: Text -> Handler TypedContent
getMinimapR name = do
  app <- getYesod
  case M.lookup name (appMinimaps app) of
    Nothing   -> notFound
    Just path -> sendFile (if ".png" `isSuffixOf` path then typePng else typeJpeg) path

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "FPS Match Viewer"
  toWidget [lucius|
    body { font-family: system-ui, sans-serif; margin: 1.5rem; }
    #filters { display: flex; gap: 1rem; flex-wrap: wrap; margin-bottom: 1rem; }
    #filters label { display: flex; flex-direction: column; font-size: .8rem; color: #555; }
    #filters select { min-width: 11rem; padding: 4px; font-size: .95rem; }
    #info { color: #666; font-size: .85rem; margin: .5rem 0; }
    #layout { display: flex; gap: 1.5rem; flex-wrap: wrap; align-items: flex-start; }
    #side { min-width: 200px; max-width: 260px; }
    #side label { display: block; margin: .25rem 0; font-size: .9rem; }
    #side h3 { margin: .75rem 0 .35rem; }
    #playerBtns button { margin-right: .4rem; }
    #playerBox { max-height: 260px; overflow: auto; }
    .tag { font-size: .7rem; color: #999; }
    #stage { width: 768px; max-width: 100%; }
    canvas { width: 100%; height: auto; border: 1px solid #888; background: #111; display: block; }
    #controls { display: flex; gap: .75rem; align-items: center; margin-top: .5rem; }
    #slider { flex: 1; }
    #clock { font-variant-numeric: tabular-nums; font-size: .9rem; min-width: 6.5rem; }
    #playBtn { min-width: 4.5rem; padding: 4px 8px; }
    #timeline { max-height: 768px; overflow: auto; min-width: 320px; }
    table { border-collapse: collapse; font-size: .85rem; }
    td, th { padding: 2px 8px; border-bottom: 1px solid #ddd; text-align: left; }
    #stats { margin-top: 1.5rem; overflow-x: auto; }
    #stats .sum { color: #444; }
    #stats tfoot td { font-weight: bold; border-top: 2px solid #999; }
  |]
  [whamlet|
    <h1>FPS Match Viewer
    <div #filters>
      <label>
        Map
        <select #mapSel>
      <label>
        Date
        <select #dateSel>
      <label>
        Player
        <select #playerSel>
      <label>
        Match
        <select #matchSel>
    <div #info>
    <div #layout>
      <div #side>
        <h3>Players
        <div #playerBtns>
          <button #allBtn type="button">All
          <button #noneBtn type="button">None
        <div #playerBox>
        <h3>Events
        <div #eventBox>
      <div #stage>
        <canvas #minimap width=1024 height=1024>
        <div #controls>
          <button #playBtn type="button" disabled>Play
          <input #slider type="range" min="0" max="0" step="0.1" value="0" disabled>
          <span #clock>00:00 / 00:00
          <select #speedSel disabled>
            <option value="1">1x
            <option value="2">2x
            <option value="5">5x
            <option value="10" selected>10x
            <option value="30">30x
      <div #timeline>
        <table>
          <thead>
            <tr>
              <th>Time
              <th>Player
              <th>Event
              <th>X
              <th>Y
              <th>Z
          <tbody #rows>
    <div #stats>
      <h2>Match stats
      <div #statsBox>
  |]
  toWidget [julius|
    const PALETTE = ["#3498db","#2ecc71","#e74c3c","#f1c40f","#9b59b6","#e67e22","#1abc9c","#ecf0f1"];
    const PLAYER_COLORS = ["#ff6b6b","#4dabf7","#69db7c","#ffd43b","#da77f2","#ffa94d",
                           "#38d9a9","#f783ac","#a9e34b","#74c0fc","#b197fc","#ced4da"];
    const $ = id => document.getElementById(id);
    const uniq = a => [...new Set(a)];
    let maps = {}, matches = [], kindColor = {}, images = {}, game = null;
    let tNow = 0, duration = 0, playing = false, lastFrame = 0, speed = 10;

    const mmss = s => {
      s = Math.floor(s);
      return String(Math.floor(s / 60)).padStart(2, "0") + ":" + String(s % 60).padStart(2, "0");
    };
    // assumption: human user ids are UUIDs; anything else is treated as a bot
    const isHuman = id => id.length === 36 && id.split("-").length === 5;
    const shortId = id => id.length > 12 ? id.slice(0, 8) + "\u2026" : id;

    function fill(sel, items, placeholder) {
      sel.innerHTML = "";
      const first = document.createElement("option");
      first.value = ""; first.textContent = placeholder; sel.appendChild(first);
      items.forEach(it => {
        const o = document.createElement("option");
        o.value = it.value; o.textContent = it.label; sel.appendChild(o);
      });
      sel.disabled = items.length === 0;
    }
    const plain = v => ({ value: v, label: v });

    async function init() {
      const idx = await (await fetch("/api/index")).json();
      idx.maps.forEach(m => { maps[m.id] = m; });
      matches = idx.matches.filter(m => maps[m.map]);
      uniq(matches.flatMap(m => Object.keys(m.kinds))).sort()
        .forEach((k, i) => { kindColor[k] = PALETTE[i % PALETTE.length]; });
      fill($("mapSel"), uniq(matches.map(m => m.map)).sort().map(plain), "Select map");
      resetFrom("date");
      $("mapSel").onchange = onMap;
      $("dateSel").onchange = onDate;
      $("playerSel").onchange = onPlayer;
      $("matchSel").onchange = onMatch;
      $("info").textContent = matches.length + " match files loaded";
    }

    // clear every filter below the one that changed
    function resetFrom(level) {
      const order = ["date", "player", "match"];
      const labels = { date: "Select date", player: "Select player", match: "Select match" };
      order.slice(order.indexOf(level)).forEach(l => fill($(l + "Sel"), [], labels[l]));
      game = null;
      setPlaying(false);
      setControls(false);
      $("playerBox").innerHTML = "";
      $("eventBox").innerHTML = "";
      $("statsBox").innerHTML = "";
      $("rows").innerHTML = "";
      redraw();
    }

    function onMap() {
      resetFrom("date");
      const map = $("mapSel").value;
      if (!map) return;
      const dates = uniq(matches.filter(m => m.map === map).map(m => m.date));
      fill($("dateSel"), dates.map(d => ({ value: d, label: d.replace("_", " ") })), "Select date");
    }

    function onDate() {
      resetFrom("player");
      const map = $("mapSel").value, date = $("dateSel").value;
      if (!date) return;
      const players = uniq(matches.filter(m => m.map === map && m.date === date).map(m => m.player));
      fill($("playerSel"), players.map(p => ({ value: p, label: shortId(p) })), "Select player");
    }

    function onPlayer() {
      resetFrom("match");
      const map = $("mapSel").value, date = $("dateSel").value, player = $("playerSel").value;
      if (!player) return;
      const list = matches.filter(m => m.map === map && m.date === date && m.player === player);
      fill($("matchSel"), list.map(m => ({
        value: m.id,
        label: m.match.slice(0, 8) + "\u2026 (" + Object.values(m.kinds).reduce((a, b) => a + b, 0) + " events)"
      })), "Select match");
      if (list.length === 1) { $("matchSel").value = list[0].id; onMatch(); }
    }

    function makePlayer(r, i, entry) {
      const ev = r.events;
      const kinds = {};
      ev.forEach(e => { kinds[e.kind] = (kinds[e.kind] || 0) + 1; });
      let dist = 0, prev = null;
      ev.forEach(e => {
        if (e.kind === "Position") {
          if (prev) dist += Math.hypot(e.x - prev.x, e.z - prev.z);
          prev = e;
        }
      });
      return {
        id: r.id, player: r.player, bot: !isHuman(r.player), events: ev, kinds: kinds,
        distance: dist, first: ev[0].ts, last: ev[ev.length - 1].ts,
        color: PLAYER_COLORS[i % PLAYER_COLORS.length], visible: r.id === entry.id
      };
    }

    async function onMatch() {
      const id = $("matchSel").value;
      if (id === "") return;
      const entry = matches.find(m => String(m.id) === id);
      const rows = await (await fetch("/api/game/" + encodeURIComponent(entry.match))).json();
      const players = rows.map((r, i) => makePlayer(r, i, entry));
      if (!players.length) return;
      if (!players.some(p => p.visible)) players[0].visible = true;
      const t0 = players.reduce((a, p) => Math.min(a, p.first), Infinity);
      const t1 = players.reduce((a, p) => Math.max(a, p.last), -Infinity);

      setPlaying(false);
      game = { entry: entry, players: players, t0: t0, kinds: [] };
      duration = t1 - t0;
      tNow = duration;
      $("slider").max = duration;
      $("slider").value = tNow;
      setControls(duration > 0);

      const humans = players.filter(p => !p.bot).length;
      $("info").textContent = entry.map + " | " + entry.date.replace("_", " ") + " | match " + entry.match
        + " | " + players.length + " players (" + humans + " human) | duration " + mmss(duration);
      renderPlayers();
      renderKinds();
      renderStats();
      redraw();
    }

    function renderPlayers() {
      const box = $("playerBox");
      box.innerHTML = "";
      game.players.forEach(p => {
        const l = document.createElement("label");
        l.innerHTML = '<input type="checkbox"' + (p.visible ? " checked" : "") + "> "
          + '<span style="color:' + p.color + '">&#9679;</span> ' + shortId(p.player)
          + (p.bot ? ' <span class="tag" title="user id is not a UUID: assumed to be a bot">bot</span>' : "")
          + ' <span class="tag">(' + p.events.length + ")</span>";
        l.querySelector("input").onchange = ev => { p.visible = ev.target.checked; redraw(); };
        box.appendChild(l);
      });
    }

    function renderKinds() {
      const box = $("eventBox");
      box.innerHTML = "";
      const totals = {};
      game.players.forEach(p => Object.keys(p.kinds).forEach(k => { totals[k] = (totals[k] || 0) + p.kinds[k]; }));
      game.kinds = Object.keys(totals).sort();
      game.kinds.forEach(k => {
        const l = document.createElement("label");
        l.innerHTML = '<input type="checkbox" value="' + k + '" checked> '
          + '<span style="color:' + kindColor[k] + '">&#9679;</span> ' + k + " (" + totals[k] + ")";
        l.querySelector("input").onchange = redraw;
        box.appendChild(l);
      });
    }

    // whole-match statistics (not affected by the time slider)
    function renderStats() {
      const ps = game.players, kinds = game.kinds;
      const humans = ps.filter(p => !p.bot).length;
      const total = ps.reduce((a, p) => a + p.events.length, 0);
      let h = '<p class="sum">' + ps.length + " players (" + humans + " human, " + (ps.length - humans)
        + " bot) \u00b7 duration " + mmss(duration) + " \u00b7 " + total + " events</p>";
      h += "<table><thead><tr><th>Player</th><th>Active</th><th>Distance</th>"
        + kinds.map(k => "<th>" + k + "</th>").join("") + "<th>Total</th></tr></thead><tbody>";
      ps.forEach(p => {
        h += "<tr><td>" + '<span style="color:' + p.color + '">&#9679;</span> ' + shortId(p.player)
          + (p.bot ? ' <span class="tag">bot</span>' : "") + "</td>"
          + "<td>" + mmss(p.last - p.first) + "</td><td>" + Math.round(p.distance) + "</td>"
          + kinds.map(k => "<td>" + (p.kinds[k] || 0) + "</td>").join("")
          + "<td>" + p.events.length + "</td></tr>";
      });
      h += "</tbody><tfoot><tr><td>All players</td><td></td><td>"
        + Math.round(ps.reduce((a, p) => a + p.distance, 0)) + "</td>"
        + kinds.map(k => "<td>" + ps.reduce((a, p) => a + (p.kinds[k] || 0), 0) + "</td>").join("")
        + "<td>" + total + "</td></tr></tfoot></table>"
        + '<p class="tag">Distance is the summed x/z movement between Position events, in world units.</p>';
      $("statsBox").innerHTML = h;
    }

    function getImage(m) {
      if (!images[m.id]) {
        const img = new Image();
        img.onload = redraw;
        img.src = m.image;
        images[m.id] = img;
      }
      return images[m.id];
    }

    function redraw() {
      const c = $("minimap"), g = c.getContext("2d");
      g.clearRect(0, 0, c.width, c.height);
      const m = maps[$("mapSel").value];
      if (!m) { $("rows").innerHTML = ""; return; }
      const img = getImage(m);
      if (img.complete && img.naturalWidth) g.drawImage(img, 0, 0, c.width, c.height);
      if (!game) return;

      const chosen = new Set([...document.querySelectorAll("#eventBox input:checked")].map(i => i.value));
      // world (x, z) -> minimap pixels; image Y is flipped
      const px = e => (e.x - m.ox) / m.scale * c.width;
      const py = e => (1 - (e.z - m.oz) / m.scale) * c.height;
      const shown = [];

      game.players.filter(p => p.visible).forEach(p => {
        const evs = p.events.filter(e => chosen.has(e.kind) && e.ts - game.t0 <= tNow);
        evs.forEach(e => shown.push({ p: p, e: e }));
        const pos = evs.filter(e => e.kind === "Position");

        g.strokeStyle = p.color; g.globalAlpha = 0.6; g.lineWidth = 2; g.beginPath();
        pos.forEach((e, i) => { if (i === 0) g.moveTo(px(e), py(e)); else g.lineTo(px(e), py(e)); });
        g.stroke();
        g.globalAlpha = 1;

        g.fillStyle = p.color;
        pos.forEach(e => { g.beginPath(); g.arc(px(e), py(e), 2.5, 0, 6.283); g.fill(); });

        // other events: filled with the event colour, outlined with the player colour
        evs.filter(e => e.kind !== "Position").forEach(e => {
          g.fillStyle = kindColor[e.kind]; g.strokeStyle = p.color; g.lineWidth = 3;
          g.beginPath(); g.arc(px(e), py(e), 8, 0, 6.283); g.fill(); g.stroke();
        });

        // ring around the player's current position
        const last = pos[pos.length - 1];
        if (last) {
          g.strokeStyle = "#fff"; g.lineWidth = 6;
          g.beginPath(); g.arc(px(last), py(last), 11, 0, 6.283); g.stroke();
          g.strokeStyle = p.color; g.lineWidth = 3;
          g.beginPath(); g.arc(px(last), py(last), 11, 0, 6.283); g.stroke();
        }
      });

      $("clock").textContent = mmss(tNow) + " / " + mmss(duration);
      shown.sort((a, b) => a.e.ts - b.e.ts);
      $("rows").innerHTML = shown.slice(-1000).map(s =>
        "<tr><td>" + mmss(s.e.ts - game.t0) + "</td>"
        + '<td><span style="color:' + s.p.color + '">&#9679;</span> ' + shortId(s.p.player) + "</td>"
        + '<td style="color:' + kindColor[s.e.kind] + '">' + s.e.kind + "</td>"
        + "<td>" + s.e.x.toFixed(1) + "</td><td>" + s.e.y.toFixed(1) + "</td><td>" + s.e.z.toFixed(1) + "</td></tr>").join("");
      if (playing || tNow < duration) $("timeline").scrollTop = $("timeline").scrollHeight;
    }

    function setControls(on) {
      $("slider").disabled = !on;
      $("playBtn").disabled = !on;
      $("speedSel").disabled = !on;
      if (!on) $("clock").textContent = "00:00 / 00:00";
    }

    function setPlaying(p) {
      playing = p;
      $("playBtn").textContent = p ? "Pause" : "Play";
      if (p) { lastFrame = performance.now(); requestAnimationFrame(tick); }
    }

    function tick(now) {
      if (!playing) return;
      tNow = Math.min(duration, tNow + (now - lastFrame) / 1000 * speed);
      lastFrame = now;
      $("slider").value = tNow;
      redraw();
      if (tNow >= duration) { setPlaying(false); return; }
      requestAnimationFrame(tick);
    }

    $("slider").oninput = () => { tNow = parseFloat($("slider").value); redraw(); };
    $("playBtn").onclick = () => {
      if (!playing && tNow >= duration) { tNow = 0; $("slider").value = 0; }
      setPlaying(!playing);
    };
    $("speedSel").onchange = () => { speed = parseFloat($("speedSel").value); };
    $("allBtn").onclick = () => {
      if (!game) return;
      game.players.forEach(p => { p.visible = true; });
      renderPlayers(); redraw();
    };
    $("noneBtn").onclick = () => {
      if (!game) return;
      game.players.forEach(p => { p.visible = false; });
      renderPlayers(); redraw();
    };

    init();
  |]

main :: IO ()
main = do
  args <- getArgs
  let root = case args of (p : _) -> p; _ -> "player_data"
  putStrLn ("Scanning " ++ root ++ " (every match file is parsed once) ...")
  matches  <- scanLibrary root
  minimaps <- findMinimaps root
  let known   = map mcId mapConfigs
      unknown = nub [meMap m | m <- matches, meMap m `notElem` known]
      noImage = [k | k <- known, k `M.notMember` minimaps]
  putStrLn ("Indexed " ++ show (length matches) ++ " match files; minimaps: "
            ++ show (map T.unpack (M.keys minimaps)))
  mapM_ (\m -> putStrLn ("WARNING: map without coordinate config (hidden in UI): " ++ T.unpack m)) unknown
  mapM_ (\m -> putStrLn ("WARNING: no minimap image found for " ++ T.unpack m)) noImage
  -- Cloud Run (and most hosts) tell the app which port to use via $PORT
  port <- maybe 3000 id . (>>= readMaybe) <$> lookupEnv "PORT"
  putStrLn ("Listening on port " ++ show port)
  warp port App
    { appMatches  = matches
    , appByMatch  = M.fromListWith (\new old -> old ++ new) [(meMatch m, [m]) | m <- matches]
    , appMinimaps = minimaps
    }
