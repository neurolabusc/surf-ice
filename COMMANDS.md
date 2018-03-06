atlasgray(overlay: integer; const filt: array of integer); Gray atlas areas. For example atlasgray(0,[3,7]) will gray-out areas 3 and 7 of the background atlas. On the other hand, atlashide(1,[2,5]) will gray-out areas 2 and 5 of the first overlay image. For example atlashide(0,[3,7]) will gray the areas 3 and 7 of the background atlas. On the other hand, atlashide(1,[2,5]) will gray areas 2 and 5 of the first overlay image.
atlashide(overlay: integer; const filt: array of integer); Hide atlas areas. For example atlashide(0,[3,7]) will hide the areas 3 and 7 of the background atlas. On the other hand, atlashide(1,[2,5]) will hide areas 2 and 5 of the first overlay image.
atlasmaxindex(overlay: integer): integer; Reports maximum region humber in specified atlas. For example, if you load the CIT168 atlas (which has 15 regions) as your background image, then atlasmaxindex(0) will return 15.
atlassaturationalpha(saturation, transparency: single); Set saturation and transparency of atlas. A desaturated atlas will appear gray, a transparent atlas will reveal the background color.
atlasstatmap(atlasname, statname: string; const indices: array of integer; const intensities: array of single); This creates a new mesh, where indexed regions are saved having the corresponding brightness from the array ''indices''. For example atlasstatmap(''jhu.mz3'','''',[1,3,9],[1.1, 4.4, 2.4]) creates a mesh where regions 1, 3, and 9 are given values 1.1, 4.4 and 2.4. If statname is provided, a mesh will be saved to disk, e.g. atlasstatmap(''jhu.mz3'',''out.mz3'',[1,3,9],[1.1, 4.4, 2.4]). nb this function is for creating maps, you can load them with loadmesh() and loadoverlay().
azimuth(degrees: integer); This command rotates the rendering.
azimuthelevation(azi, elev: integer); Sets the viewer location.
backcolor(r,g,b: byte); Changes the background color, for example backcolor(255, 0, 0) will set a bright red background
cameradistance(z: single); Sets the viewing distance from the object.
camerapan(x, y: single); Translate image horizontally (x) and vertically (y). range -1..+1, where 0 is centered.
clip(depth: single); Creates a clip plane that hides information close to the viewer.
clipazimuthelevation(depth, azi, elev: single); Set a view-point independent clip plane.
closeall(); Closes all open meshes, overlays, nodes and tracks
colorbarposition(pos: integer); Sets the position of the colorbar: 1=bottom, 2=left, 3=top, 4=right.
colorbarvisible(visible: boolean); Shows a colorbar on the main images.
createedge(filename: string; const mtx: array of single); Create a connectome edge map. For example, if you have 3 nodes, mtx should be an array with 9 values (3*3). For example, edgecreate('''',[1,2,3, 2,4,5, 3,5,6]) creates the links between 3 regions. Note the lower triangle and diagonal of this matrix is ignored.
edgecolor(name: string; varies: boolean); Select color scheme for connectome edge map
edgeload(filename: string); Loads a BrainNet Viewer format Edge file, e.g. connectome map
edgesize(size: single; varies: boolean);
edgethresh(lo, hi: single); Set minimum and maximum values for connectome edge diameters.
elevation(degrees: integer); changes the render camera up or down.
meshcolor(R,G,B: byte); Set red/green/blue components of main image.
meshcurv(); Displays mesh curvature, so crevices appear dark.
meshload(filename: string); Opens a mesh to view.
modalmessage(str: string); Shows a modal dialog, script stops until user presses ''OK'' button to dismiss dialog.
modelessmessage(str: string); Shows text in the rendering window. This text is displayed until the text is changed.
nodecolor(name: string; varies: boolean);
nodecreate(filename: string; const x, y, z, color, radius: array of single); Generates and displays a connectome hub map. The arrays x,y,z refer to the 3D spatial coordinates. The optional color and radius arrays allow you to specify the size. For example nodecreate(''mynode.node'',[0, 10, 8], [9, 0, 8], [0, 9, 0], [], []) creates three spheres with the default size and color. On the other hand, nodecreate(''mynode.node'',[0, 10, 8], [9, 0, 8], [0, 9, 0], [1, 2, 9], [5]) creates three spheres each with a unique color intensity and with a size of 5.
nodehemisphere(val: integer); -1 for left hemipshere, 0 for both, 1 for right
nodeload(filename: string); Loads BrainNet viewer format node file.
nodepolarity(val: integer); -1 for negative only, 0 for either, 1 for positive only
nodesize(size: single; varies: boolean); Determine size scaling factor for nodes.
nodethresh (lo, hi: single);
nodethreshbysizenotcolor(NodeThresholdBySize: boolean); If true then nodes will be hidden if they are smaller than the provided threshold. If false, they will be hidden if their color intensity is below the provided threshold.
orientcubevisible(visible: boolean); show or hide cube that indicates object rotation
overlayadditive(add: boolean); Determines whether overlay colors are combined by adding or mixing the colors. For example, overlap of red and green overlays will appear yellow if additive is true
overlaycloseall(); This function has no parameters. All open overlays will be closed.
overlaycolorfromzero(fromzero: boolean); If set to false, then the full color range is used to show the overlay.
overlaycolorname(overlay: integer; filename: string); Set the colorscheme for the target overlay to a specified name.
overlayinvert(overlay: integer; invert: boolean); toggle whether overlay color scheme is inverted.
overlayload(filename: string): integer; Will add the overlay named filename and return the number of the overlay.
overlayminmax(overlay: integer; min, max: single); Sets the color range for the overlay.
overlaysmoothvoxelwisedata(smooth: boolean); Determines if overlays are loaded using interpolation (smooth) or nearest neighbor (un-smoothed) interpolation.
overlaytranslucent(overlay: integer; translucent: boolean); The feature allows you to make individual overlays translucent or opaque.
overlaytransparencyonbackground(percent: integer); Controls the opacity of the overlays on the background.
overlayvisible(overlay: integer; visible: boolean); The feature allows you to make individual overlays visible or invisible.
QUIT Terminate application. Useful if another program is controlling this software
resetdefaults(); Sets all of the user adjustable settings to their default values.
savebmp(filename: string); Saves the currently viewed image as a PNG format compressed bitmap image.
shaderadjust(property: string; value: single); Sets one of the user-adjustable properties.
shaderambientocclusion(amount: single); range 0..1: strength of crevice shadows
shaderforbackgroundonly(onlybg: boolean); If true selected shader only influeces background image, otherwise shader influences background, overlays, tracks and nodes.
shaderlightazimuthelevation(azimuth, elevation: integer); Changes location of light source.
shadername(Filename: string); Loads the requested shader.
shaderxray(object, overlay: single);  See occluded overlays/tracks/nodes by making either object transparent (0..1) or overlay/tracks/nodes emphasized (0..1)
trackload(filename: string);
trackprefs(length, width, dither: single);
viewaxial(std: boolean); creates rendering from an axial viewpoint.
viewcoronal(std: boolean); creates rendering from a coronal viewpoint.
viewsagittal(std: boolean); creates rendering from an sagittal viewpoint.
wait(msec: integer); The program pauses for the specified duration. For example wait(1000) delays the script for one second.'
