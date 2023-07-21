## MANUALLY FIXING INDIANA HYDROGRAPHY
Upon visual insepction, we found that the Indiana NHD-HR hydrography drainage density is artifically dense and bounded by the state's border. To fix this, we manually verified the general headwater drainage area for the non-Indiana portion of the drainage networks, and set stream order thresholds, usually a little below the median, to 're-extract' reasonable river networks for Indiana. 'Reasonable river networks' were determined visually by plotting the river networks adjacent to these basins and confirming that the drainage densities were approximately similar.

### GUIDELINES
HUC4: 0508
 StreamOrder > 2 (3+)

HUC4: 0509:
 StreamOrder > 2 (3+)

HUC4: 0514:
 StreamOrder > 2 (3+)

HUC4: 0512:
 StreamOrder > 2 (3+)

HUC4: 0712
 StreamOrder > 3 (4+)

HUC4: 0404:
 StreamOrder > 2 (3+)

HUC4: 0405:
 StreamOrder > 3 (4+)

HUC4: 0410:
 StreamOrder > 2 (3+)