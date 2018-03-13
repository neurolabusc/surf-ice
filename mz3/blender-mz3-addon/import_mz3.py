#####
#
# Copyright 2018 Chris Rorden
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
#####

#
# http://wiki.blender.org/index.php/Dev:2.5/Py/Scripts/Guidelines/Addons
#
import os
import sys
import struct
import gzip
import array
import bpy
import mathutils
from bpy.props import (BoolProperty,
        FloatProperty,
        StringProperty,
        EnumProperty,
    )
from bpy_extras.io_utils import (ImportHelper,
    ExportHelper,
    unpack_list,
    unpack_face_list,
    axis_conversion,
    )

#if "bpy" in locals():
#    import imp
#    if "import_mz3" in

bl_info = {
    "name": "MZ3 format",
    "description": "Import-Export MZ3, Import/export simple MZ3 mesh.",
    "author": "Chris Rorden",
    "version": (0, 3),
    "blender": (2, 74, 0),
    "location": "File > Import-Export",
    "warning": "", # used for warning icon and text in addons panel
    "wiki_url": "http://wiki.blender.org/index.php/Extensions:2.5/Py/"
                "Scripts/My_Script",
    "category": "Import-Export"}

class ImportMZ3(bpy.types.Operator, ImportHelper):
    """Load an MZ3 Mesh file"""
    bl_idname = "import_mesh.mz3"
    bl_label = "Import MZ3 Mesh"
    filename_ext = ".mz3"
    filter_glob = StringProperty(
        default="*.mz3",
        options={'HIDDEN'},
    )

    axis_forward = EnumProperty(
            name="Forward",
            items=(('X', "X Forward", ""),
                   ('Y', "Y Forward", ""),
                   ('Z', "Z Forward", ""),
                   ('-X', "-X Forward", ""),
                   ('-Y', "-Y Forward", ""),
                   ('-Z', "-Z Forward", ""),
                   ),
            default='Y',
            )
    axis_up = EnumProperty(
            name="Up",
            items=(('X', "X Up", ""),
                   ('Y', "Y Up", ""),
                   ('Z', "Z Up", ""),
                   ('-X', "-X Up", ""),
                   ('-Y', "-Y Up", ""),
                   ('-Z', "-Z Up", ""),
                   ),
            default='Z',
            )

    def execute(self, context):
        #from . import import_mz3

        keywords = self.as_keywords(ignore=('axis_forward',
            'axis_up',
            'filter_glob',
        ))
        global_matrix = axis_conversion(from_forward=self.axis_forward,
            from_up=self.axis_up,
            ).to_4x4()

        mesh = load(self, context, **keywords)
        if not mesh:
            return {'CANCELLED'}

        scene = bpy.context.scene
        obj = bpy.data.objects.new(mesh.name, mesh)
        scene.objects.link(obj)
        scene.objects.active = obj
        obj.select = True

        obj.matrix_world = global_matrix

        scene.update()

        return {'FINISHED'}

class ExportMZ3(bpy.types.Operator, ExportHelper):
    """Save an MZ3 Mesh file"""
    bl_idname = "export_mesh.mz3"
    bl_label = "Export MZ3 Mesh"
    filter_glob = StringProperty(
        default="*.mz3",
        options={'HIDDEN'},
    )
    check_extension = True
    filename_ext = ".mz3"

    axis_forward = EnumProperty(
            name="Forward",
            items=(('X', "X Forward", ""),
                   ('Y', "Y Forward", ""),
                   ('Z', "Z Forward", ""),
                   ('-X', "-X Forward", ""),
                   ('-Y', "-Y Forward", ""),
                   ('-Z', "-Z Forward", ""),
                   ),
            default='Y',
            )
    axis_up = EnumProperty(
            name="Up",
            items=(('X', "X Up", ""),
                   ('Y', "Y Up", ""),
                   ('Z', "Z Up", ""),
                   ('-X', "-X Up", ""),
                   ('-Y', "-Y Up", ""),
                   ('-Z', "-Z Up", ""),
                   ),
            default='Z',
            )
    use_colors = BoolProperty(
            name="Vertex Colors",
            description="Export the active vertex color layer",
            default=False,
            )

    def execute(self, context):
        keywords = self.as_keywords(ignore=('axis_forward',
            'axis_up',
            'filter_glob',
            'check_existing',
        ))
        global_matrix = axis_conversion(to_forward=self.axis_forward,
            to_up=self.axis_up,
            ).to_4x4()
        keywords['global_matrix'] = global_matrix
        return save(self, context, **keywords)

def menu_func_import(self, context):
    self.layout.operator(ImportMZ3.bl_idname, text="MZ3 Mesh (.mz3)")

def menu_func_export(self, context):
    self.layout.operator(ExportMZ3.bl_idname, text="MZ3 Mesh (.mz3)")

def register():
    bpy.utils.register_module(__name__)
    bpy.types.INFO_MT_file_import.append(menu_func_import)
    bpy.types.INFO_MT_file_export.append(menu_func_export)

def unregister():
    bpy.utils.unregister_module(__name__)
    bpy.types.INFO_MT_file_import.remove(menu_func_import)
    bpy.types.INFO_MT_file_export.remove(menu_func_export)

def load(operator, context, filepath):
    # Parse mesh from MZ3 file
    filepath = os.fsencode(filepath)
    isVerbose = False
    use_colors = False
    #edges = []
    faces = []
    verts = []
    colors = []
    scalar = []
    with open(filepath, 'rb') as f:
        hdrBytes = 16
        data = f.read(hdrBytes)
        # header is 16 bytes LITTLE-ENDIAN
        #  UINT16 : MAGIC
        #  UINT16 : ATTR
        #  UINT32 : NFACE
        #  UINT32 : NVERT
        #  UINT32 : NSKIP
        hdr = struct.unpack_from('<HHIII', data)
        if hdr[0] != 23117:  # incorrect magic: assume gzip
            f = gzip.open(filepath, 'rb')
            data = f.read(hdrBytes)
            hdr = struct.unpack_from('<HHIII', data)
            if hdr[0] != 23117:  # incorrect magic: not mz3
                print("Not a valid MZ3 file")
                return invalid_mz3
            if isVerbose:
                print("GZ Compressed")
        MAGIC = hdr[0]
        ATTR = hdr[1]
        NFACE = hdr[2]
        NVERT = hdr[3]
        NSKIP = hdr[4]
        # read attributes
        isFACE = (ATTR & 1) != 0
        isVERT = (ATTR & 2) != 0
        isRGBA = (ATTR & 4) != 0
        use_colors = isRGBA
        isSCALAR = (ATTR & 8) != 0
        # report contents
        if isVerbose:
            print("MAGIC %d ATTR %d" % (MAGIC, ATTR))
            print("NFACE %d NVERT %d NSKIP %d" % (NFACE, NVERT, NSKIP))
            print(" isFACE %r isVERT %r" % (isFACE, isVERT))
            print(" isRGBA %r isSCALAR %r" % (isRGBA, isSCALAR))
        # quit if file does not make sense
        if ATTR > 15:
            print("Unable to read future version of MZ3 file")
            return invalid_mz3
        if NVERT < 1:
            print("Unable to read MZ3 files without vertices")
            return invalid_mz3
        if (NFACE < 1) & (isFACE):
            print("MZ3 files with isFACE must specify NFACE")
            return invalid_mz3
        # read faces
        f.seek(hdrBytes+NSKIP)
        if isFACE:
            for i in range(NFACE):
                data = f.read(12)   # each face is 3 UINT32 vertex indices
                faces.append(struct.unpack_from('<3I', data))
            #data = f.read(12 * NFACE)  # each face is 3 UINT32 vertex indices
            #str = '<{}I'.format(3*NFACE)
            #faces = struct.unpack_from(str, data)
        # read vertices
        f.seek(hdrBytes+NSKIP+(isFACE * NFACE * 3 * 4))
        if isVERT:
            for i in range(NVERT):
                data = f.read(12)  # each vertex is 3 FLOAT32 (xyz)
                verts.append(struct.unpack_from('<3f', data))
        # read RGBA colors
        f.seek(hdrBytes+NSKIP+(isFACE * NFACE * 12)+(isVERT * NVERT * 12))
        if isRGBA:
            for i in range(NVERT):
                data = f.read(4)  # each vertex has UINT32 RGBA value
                clr = struct.unpack_from('<4B', data)
                colors.append([float(clr[0]) / 255, float(clr[1]) / 255, float(clr[2]) / 255 ] )
            #data = f.read(4 * NVERT)  # each vertex has UINT32 RGBA value
            #str = '<{}I'.format(NVERT)
            #colors = struct.unpack_from(str, data)
        # read Scalar Intensity
        f.seek(hdrBytes+NSKIP+(isFACE * NFACE * 12)
               + (isVERT * NVERT * 12)+(isRGBA * NVERT * 4))
        if isSCALAR:
            for i in range(NVERT):
                data = f.read(4)  # each vertex has FLOAT32 scalar value
                scalar.append(struct.unpack_from('<1f', data))
            #data = f.read(4 * NVERT)  # each vertex has FLOAT32 scalar value
            #str = '<{}f'.format(NVERT)
            #scalar = struct.unpack_from(str, data)
    # Assemble mesh
    mz3_name = bpy.path.display_name_from_filepath(filepath)
    mesh = bpy.data.meshes.new(name=mz3_name)
    #mesh.from_pydata(verts,edges,faces)
    mesh.from_pydata(verts,[],faces)
    # mesh.vertices.add(len(verts))
    # mesh.vertices.foreach_set("co", unpack_list(verts))

    # mesh.faces.add(len(faces))
    # mesh.faces.foreach_set("vertices", unpack_face_list(faces))

    mesh.validate()
    mesh.update()

    if use_colors:
        color_data = mesh.vertex_colors.new()
        for i, facet in enumerate(mesh.polygons):
            for j, vidx in enumerate(facet.vertices):
                color_data.data[3*i + j].color = colors[vidx]
    return mesh

def writeMZ3(filepath, faces, verts, colors):
    isFACE = len(faces) > 0
    isVERT = len(verts) > 0
    isRGBA = len(colors) > 0
    MAGIC = 23117
    NFACE = len(faces)
    NVERT = len(verts)
    ATTR = 0
    if (isFACE):
        ATTR = ATTR + 1
    if (isVERT):
        ATTR = ATTR + 2
    if (isRGBA):
        ATTR = ATTR + 4
    NSKIP = 0
    filepath = os.fsencode(filepath)
    with gzip.open(filepath, 'wb') as f:  # <-- compressed
    #with open(filepath, 'wb') as f:  # <-- uncompressed
        f.write(struct.pack('<H', MAGIC))
        f.write(struct.pack('<H', ATTR))
        f.write(struct.pack('<I', NFACE))
        f.write(struct.pack('<I', NVERT))
        f.write(struct.pack('<I', NSKIP))
        if (isFACE):
            for i, facet in enumerate(faces):
            	#if (len(facet.vertices) != 3): error
                for vid in facet.vertices:
                    f.write(struct.pack('<1I', int( vid)))
        if (isVERT):
            for i, vert in enumerate(verts):
                f.write(struct.pack('<3f', float(vert.co[0]), float(vert.co[1]), float(vert.co[2])))
                #print("%.16f %.16f %.16f" % vert.co[:])
        if (isRGBA):
            for i, vc in enumerate(colors):
            	f.write(struct.pack('<4B', int(vc[0]), int(vc[1]), int(vc[2]), 255))

def save(operator, context, filepath,
    global_matrix = None,
    use_colors = False):
    if global_matrix is None:
        global_matrix = mathutils.Matrix()
    # Export the selected mesh
    obj = bpy.context.scene.objects.active
    bpy.context.scene.objects.active = obj
    bpy.ops.object.modifier_add(type='TRIANGULATE')  # no 4-gons or n-gons
    mesh = obj.to_mesh(bpy.context.scene, True, 'PREVIEW')
    #APPLY_MODIFIERS = True # TODO: Make this configurable
    #scene = context.scene
    #obj = scene.objects.active
    #mesh = obj.to_mesh(scene, APPLY_MODIFIERS, 'PREVIEW')
    bpy.ops.object.mode_set(mode='EDIT', toggle=False)
    bpy.ops.mesh.quads_convert_to_tris(quad_method='BEAUTY', ngon_method='BEAUTY')
    bpy.ops.object.mode_set(mode='OBJECT', toggle=False)
    # Apply the inverse transformation
    obj_mat = obj.matrix_world
    mesh.transform(global_matrix * obj_mat)
    #export data
    verts = mesh.vertices[:]
    facets = [ f for f in mesh.tessfaces ]
    # Collect colors by vertex id
    colors = False
    vertex_colors = []
    if use_colors:
        colors = mesh.tessface_vertex_colors.active
    if colors:
        colors = colors.data
        vertex_colors = {}
        for i, facet in enumerate(mesh.tessfaces):
            color = colors[i]
            color = color.color1[:], color.color2[:], color.color3[:], color.color4[:]
            for j, vidx in enumerate(facet.vertices):
                if vidx not in vertex_colors:
                    vertex_colors[vidx] = (int(color[j][0] * 255.0),
                                            int(color[j][1] * 255.0),
                                            int(color[j][2] * 255.0))
    writeMZ3(filepath, facets, verts, vertex_colors)
    return {'FINISHED'}


if __name__ == "__main__":
    register()
