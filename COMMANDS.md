### Functions

Running the `help` menu item listed in the `Scripting` Menu's `Python` menu will generate a comprehensive list of the available functions. Here is the output for v1.0.20201102:

 - atlas2node (built-in function): 
 atlas2node(imageName) -> convert .annot file labels to BrainNet Node format.
 - atlashide (built-in function): 
 atlashide(overlayNum, (r1, r2, ...)) -> Hide regions specified atlas. For example, "atlashide(0, (3, 7, 9))" will hide regions 3,7,9 of the background (0th layer) image.
 - atlasmaxindex (built-in function): 
 atlasmaxindex(overlayNum) -> Returns maximum region humber in specified atlas. For example, if you load the CIT168 atlas (which has 15 regions) as your background image, then atlasmaxindex(0) will return 15.
 - atlassaturationalpha (built-in function): 
 atlassaturationalpha(saturation, transparency) -> Set saturation and transparency of atlas. A desaturated atlas will appear gray, a transparent atlas will reveal the background color.
 - atlasshow (built-in function): 
 atlasshow(overlayNum, (r1, r2, ...)) -> Show regions specified atlas. For example, "atlasshow(1, (3, 7, 9))" will show regions 3,7,9 of the first overlay image.
 - atlasstatmap (built-in function): 
 atlasstatmap(atlasname, outname, (r1, r2, ...), (i1, i2, ...)) -> Create mesh named "outname" where regions have specified intensities.
 - azimuth (built-in function): 
 azimuthe(azi) -> Rotate image by specified degrees.
 - azimuthelevation (built-in function): 
 azimuthelevation(azi, elev) -> Sets the camera location.
 - backcolor (built-in function): 
 backcolor(r, g, b) -> changes the background color, for example backcolor(255, 0, 0) will set a bright red background
 - bmpzoom (built-in function): 
 bmpzoom(z) -> changes resolution of savebmp(), for example bmpzoom(2) will save bitmaps at twice screen resolution
 - cameradistance (built-in function): 
 cameradistance(z) -> Sets the viewing distance from the object.
 - camerapan (built-in function): 
 camerapan(x, y) -> Translate image horizontally (x) and vertically (y). range -1..+1, where 0 is centered.
 - clip (built-in function): 
 clip(depth) -> Creates a clip plane that hides information close to the viewer.
 - clipazimuthelevation (built-in function): 
 clipazimuthelevation(depth, azi, elev) -> Set a view-point independent clip plane.
 - colorbarposition (built-in function): 
 colorbarposition(p) -> Set colorbar position (1=bottom, 2=left, 3=top, 4=right).
 - colorbarvisible (built-in function): 
 colorbarvisible(v) -> Show (1) or hide (0) the color bar.
 - contour (built-in function): 
 contour(layer) -> Create edge map for atlas or overlay.
 - edgecolor (built-in function): 
 edgecolor(name, varies) -> Select color scheme for connectome edge map. If varies=1 then edge color depends on strength of connection.
 - edgeload (built-in function): 
 edgeload(filename) -> Loads a BrainNet Viewer format Edge file, e.g. connectome map.
 - edgesize (built-in function): 
 edgesize (size, varies) -> Set the diameters of the cylinders of the connectome. If varies=1 then edge diameter depends on strength of connection.
 - edgethresh (built-in function): 
 edgethresh (lo, hi) -> Set minimum and maximum values for connectome edge diameters.
 - elevation (built-in function): 
 elevation(degrees) -> Rotates volume rendering relative to camera.
 - exists (built-in function): 
 exists(filename) -> Returns true if filename is found.
 - fontname (built-in function): 
 fontname(name) -> Set typeface for display.
 - fullscreen (built-in function): 
 fullscreen(max) -> Form expands to size of screen (1) or size is maximized (0).
 - hemispheredistance (built-in function): 
 hemispheredistance(v) -> Grow or shrink space between left and right hemisphere (-1..1).
 - hemispherepry (built-in function): 
 hemispherepry(degrees) -> Rotate hemispheres relative to each other, the SUMA "walnut pry" effect.
 - meshcolor (built-in function): 
 meshcolor(r, g, b) -> Set red/green/blue components of main image. Each component is an integer 0..255.
 - meshcreate (built-in function): 
 meshcreate(niiname, meshname, threshold, decimateFrac, minimumClusterVox, smoothStyle) -> Convert a NIfTI voxel-based image into a mesh.
 - meshcurv (built-in function): 
 meshcurv() -> Displays mesh curvature, so crevices appear dark.
 - meshhemisphere (built-in function): 
 meshhemisphere(v) -> nodehemisphere (val) -> Set -1 for left hemipshere, 0 for both, 1 for right.
 - meshload (built-in function): 
 meshload(imageName) -> Close all open images and load new background image.
 - meshloadbilateral (built-in function): 
 meshloadbilateral(v) -> If v=1, load both hemispheres (*.lh *.rh), otherwise only load named mesh.
 - meshoverlayorder (built-in function): 
 meshoverlayorder (flip) -> If flip=1, the mesh will be drawn after the overlay, and xray sliders will influence overlay not mesh.
 - meshreversefaces (built-in function): 
 meshreversefaces() -> reverse triangle winding to reverse front/back faces.
 - meshsave (built-in function): 
 meshsave(filename) -> Saves currently open mesh to disk.
 - modalmessage (built-in function): 
 modalmessage(msg) -> Shows a modal dialog, script stops until user presses 'OK' button to dismiss dialog.
 - modelessmessage (built-in function): 
 modelessmessage(msg) -> Prints text in the bottom status region of the scripting window.
 - nodecolor (built-in function): 
 nodecolor(name, varies) -> set colorscheme used for nodes. If varies=1, the color of nodes will differ depending on size or intensity.
 - nodehemisphere (built-in function): 
 nodehemisphere (val) -> Set -1 for left hemipshere, 0 for both, 1 for right.
 - nodeload (built-in function): 
 nodeload(filename) -> Loads BrainNet viewer format node file.
 - nodepolarity (built-in function): 
 nodepolarity(val) -> Set -1 for negative only, 0 for either, 1 for positive only.
 - nodesize (built-in function): 
 nodesize(size, varies) -> Determine size scaling factor for nodes.
 - nodethresh (built-in function): 
 nodethresh(lo, hi) -> Set the minimum and maximum range for nodes.
 - nodethreshbysizenotcolor (built-in function): 
 nodethreshbysizenotcolor(NodeThresholdBySize) -> If true (1) then nodes will be hidden if they are smaller than the provided threshold. If false (0), they will be hidden if their color intensity is below the provided threshold.
 - orientcubevisible (built-in function): 
 orientcubevisible (visible) -> Show (1) or hide (0) cube that indicates object rotation
 - overlayadditive (built-in function): 
 overlayadditive (add) -> Determines whether overlay colors are combined by adding or mixing the colors. For example, overlap of red and green overlays will appear yellow if additive is true (1)
 - overlaycloseall (built-in function): 
 overlaycloseall() -> Close all open overlays.
 - overlaycolor (built-in function): 
 overlaycolor(overlayLayer, loR, loG, loB, hiR, hiG, hiB) -> Set the colorscheme (low and high Red, Green, Blue) for the target overlay.
 - overlaycolorname (built-in function): 
 overlaycolorname(overlayLayer, filename) -> Set the colorscheme for the target overlay to a specified name.
 - overlaycount (built-in function): 
 overlaycount() -> Return number of overlays currently open.
 - overlayextreme (built-in function): 
 overlayextreme(layer, mode) -> Behavior of values beyond min..max (-1 automatic, 0 hide dark & bright, 1: hide dark, 2 hide bright, 3 show dark & bright)
 - overlayinvert (built-in function): 
 overlayinvert(layer, invert) -> Toggle whether overlay color scheme is inverted.
 - overlayload (built-in function): 
 overlayload(filename) -> Load an image on top of prior images.
 - overlayminmax (built-in function): 
 overlayminmax(layer, min, max) -> Sets the color range for the overlay (layer 0 = background).
 - overlayopacity (built-in function): 
 overlayopacity(overlayLayer, opacity) -> This feature allows you to adjust individual overlays transparency from transparent (0) to opaque (100).
 - overlayoverlapoverwrite (built-in function): 
 overlayoverlapoverwrite(overwrite) -> Does an overlapping overlay overwrite existing color (1) or are the colors blanded (0).
 - overlaysmoothvoxelwisedata (built-in function): 
 overlaysmoothvoxelwisedata(smooth) -> Determines if overlays are loaded using interpolation (smooth, 1) or nearest neighbor (un-smoothed, 0) interpolation.
 - overlaytranslucent (built-in function): 
 overlaytranslucent(overlayLayer, translucent) -> This feature allows you to make individual overlays translucent or opaque.
 - overlaytransparencyonbackground (built-in function): 
 overlaytransparencyonbackground(percent) -> Controls the opacity of the overlays on the background.
 - overlayvisible (built-in function): 
 overlayvisible(overlayLayer, visible) -> This feature allows you to make individual overlays visible or invisible.
 - pitch (built-in function): 
 pitch(degrees) -> Sets the pitch of object to be rendered.
 - quit (built-in function): 
 quit() -> Terminate the application.
 - resetdefaults (built-in function): 
 resetdefaults() -> Revert settings to sensible values.
 - savebmp (built-in function): 
 savebmp(pngName) -> Save screen display as bitmap. For example "savebmp('test.png')"
 - savebmpxy (built-in function): 
 savebmpxy(pngName, x, y) -> Saves the currently viewed image as a PNG bitmap image. Specify the image width (x) and height (y).
 - scriptformvisible (built-in function): 
 scriptformvisible (visible) -> Show (1) or hide (0) the scripting window.
 - shaderadjust (built-in function): 
 shaderadjust(sliderName, sliderValue) -> Set level of shader property. Example "gl.shaderadjust('Diffuse', 0.6)"
 - shaderambientocclusion (built-in function): 
 shaderambientocclusion(amount) -> Specify a value in the range 0..1 to set the strength of the crevice shadows
 - shaderforbackgroundonly (built-in function): 
 shaderforbackgroundonly(onlybg) -> If true (1) selected shader only influeces background image, otherwise shader influences background, overlays, tracks and nodes.
 - shaderlightazimuthelevation (built-in function): 
 shaderlightazimuthelevation (azimuth, elevation) -> Changes location of light source.
 - shadermatcap (built-in function): 
 shadermatcap(name) -> Set material capture file (assumes "matcap" shader. For example, "shadermatcap('mc01')" selects mc01 matcap.
 - shadername (built-in function): 
 shadername(name) -> Choose rendering shader function. For example, "shadername('phong')" renders using Phong shading.
 - shaderxray (built-in function): 
 shaderxray (object, overlay) -> See occluded overlays/tracks/nodes by making either object transparent (0..1) or overlay/tracks/nodes emphasized (0..1)
 - trackload (built-in function): 
 trackload (filename) -> Load fiber steam lines from a file.
 - trackprefs (built-in function): 
 trackprefs(length, width, dither) -> Set the size and properties for streamlines.
 - version (built-in function): 
 version() -> Return the version of Surfice.
 - viewaxial (built-in function): 
 viewaxial(SI) -> Show rendering with camera superior (1) or inferior (0) of volume.
 - viewcoronal (built-in function): 
 viewcoronal(AP) -> Show rendering with camera posterior (1) or anterior (0) of volume.
 - viewsagittal (built-in function): 
 viewsagittal(LR) -> Show rendering with camera left (1) or right (0) of volume.
 - wait (built-in function): 
 wait(ms) -> Pause script for (at least) the desired milliseconds.
Python Succesfully Executed
