# fps-viewer

    cabal build
    cabal run fps-viewer -- /path/to/player_data     # default: ./player_data

Open http://localhost:3000

Expected folder layout:

    player_data/
      February_10/<user-id>_<match-id>.nakama-0
      February_11/...
      minimaps/AmbroseValley_Minimap.png, GrandRift_Minimap.png, Lockdown_Minimap.jpg

Filters cascade: Map -> Date -> Player -> Match -> Events.

Routes: 
/  (UI)  
/api/index  
/api/game/<match-id>  
/minimap/<MapId>

Code contains:
1) Parquet parser  
2) index + server + cascading UI  
3) replay slider  
4) multi-player overlay + match stats
