# fps-viewer

NATIVE BUILD:
    cabal build
    cabal run fps-viewer -- /path/to/player_data     # default: ./player_data

SURF:
    https://fps-viewer-787416256634.europe-west1.run.app/

Expected folder layout:
    player_data/
      February_10/<user-id>_<match-id>.nakama-0
      February_11/...
      minimaps/AmbroseValley_Minimap.png, GrandRift_Minimap.png, Lockdown_Minimap.jpg

Filters cascade:
    Map -> Date -> Player -> Match -> Events.

Routes: 
		/  (UI)  
		/api/index  
		/api/game/<match-id>  
		/minimap/<MapId>

Code contains:
		Parquet parser  
		index + server + cascading UI  
		replay slider  
	  multi-player overlay + match stats
