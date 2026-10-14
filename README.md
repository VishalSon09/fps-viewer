# FPS VIEWER 

NATIVE BUILD:

    cabal build
    cabal run fps-viewer -- /path/to/player_data     # default: ./player_data

		http://localhost:3000

SURF:

    https://fps-viewer-787416256634.europe-west1.run.app/


FILTERS CASCADE:

    Map -> Date -> Player -> Match -> Events.

ROUTES: 

		/  (UI)  
		/api/index  
		/api/game/<match-id>  
		/minimap/<MapId>

CODE CONTAINS:

		Parquet parser  
		index + server + cascading UI  
		replay slider  
	  multi-player overlay + match stats
