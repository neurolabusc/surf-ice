/**
 * @author Chris Rorden
 * See https://github.com/kchapelier/MZ3 for more informations about this file format
 */

( function ( THREE ) {

	'use strict';

	function decodeMZ3( buffer ) {
		if (buffer.byteLength < 20) //76 for raw, not sure of gzip
			throw new Error( 'File to small to be mz3: bytes = '+ buffer.byteLength);

		var reader = new DataView(buffer);
		//get number of vertices and faces
		var magic = reader.getUint16(0, true);
		var _buffer = buffer;

		if ((magic === 35615) || (magic === 8075)) { //gzip signature 0x1F8B in little and big endian
			//console.log("detected gzipped mz3");
			//HTML should source an inflate script:
			// <script src="https://cdn.jsdelivr.net/pako/1.0.3/pako.min.js"></script>
			// <script src="js/libs/gunzip.min.js"></script>
			//for decompression there seems to be little real world difference
			var raw;
			if ((typeof pako === "object") && (typeof pako.deflate === "function")) {
				raw = pako.inflate(new Uint8Array(buffer));
			} else if ((typeof Zlib === "object") && (typeof Zlib.Gunzip === "function")) {
				var inflate = new Zlib.Gunzip( new Uint8Array( buffer ) ); // eslint-disable-line no-undef
				raw = inflate.decompress();
			} else
				alert("Required script missing: include either pako.min.js or gunzip.min.js");;
			//console.log("gz->raw %d->%d", buffer.byteLength, raw.length);
			var reader = new DataView(raw.buffer);
			var magic = reader.getUint16(0, true);
			_buffer = raw.buffer;
			//throw new Error( 'Gzip MZ3 file' );
		}
		var attr = reader.getUint16(2, true);
		var nface = reader.getUint32(4, true);
		var nvert = reader.getUint32(8, true);
		var nskip = reader.getUint32(12, true);
		console.log("magic %d attr %d face %d vert %d skip %d", magic, attr, nface, nvert, nskip);
		if (magic != 23117)
			throw new Error( 'Invalid MZ3 file' );
		var isFace = attr & 1;
		var isVert = attr & 2;
		var isRGBA = attr & 4;
		var isSCALAR = attr & 8;
		var isDOUBLE = attr & 16;
		var isAOMap = attr & 32;
		if (attr > 63)
			throw new Error("Unsupported future version of MZ3 file");
		if ((!isFace) || (!isVert) || (nface < 1) || (nvert < 3))
			throw new Error("Not a mesh MZ3 file (maybe scalar)");
		var filepos = 16 + nskip;
		var indices = null;
		if (isFace) {
			indices = new Uint32Array(_buffer, filepos, nface*3, true);
			filepos += nface*3*4;
		}
		var positions = null;
		if (isVert) {
			positions = new Float32Array(_buffer, filepos,nvert*3, true);
			filepos += nvert*3*4;
		}
		var colors = null;
		if (isRGBA) {
			colors = new Float32Array(nvert * 3);
			var rgba8 = new Uint8Array(_buffer, filepos,nvert*4, true);
			filepos += nvert*4;
			//var i, j;
			var k3 = 0;
			var k4 = 0;
			for ( var i = 0; i < nvert; i ++ ) {
				for ( var j = 0; j < 3; j ++ ) { //for RGBA
					colors[k3] = rgba8[k4] / 255;
					k3++;
					k4++;
				}
				k4++; //skip Alpha
			} //for i
		} //if isRGBA
		//
		var uv2 = null;
		if ((!isRGBA) && (isSCALAR) && (isAOMap)) {
			var scalars = new Float32Array(_buffer, filepos,nvert, true);
			filepos += nvert*4;
			/*var mn = scalars[0];
			var mx = scalars[0];

			for ( var i = 0; i < nvert; i ++ ) {
				if (scalars[i] < mn) mn = scalars[i];
				if (scalars[i] > mx) mx = scalars[i];

			}
			console.log("scalar range %g...%g", mn, mx);*/
			uv2 = new Float32Array( nvert * 2 );
			for ( var i = 0; i < nvert; i ++ ) {
				uv2[ i * 2 ] = uv2[ i * 2 + 1 ] = scalars[i];
			}


		}
		return {
			positions: positions,
			indices: indices,
			uv2: uv2,
			colors: colors
		};
	}

	// Define the public interface

	THREE.MZ3Loader = function MZ3Loader( manager ) {
		this.manager = ( manager !== undefined ) ? manager : THREE.DefaultLoadingManager;
	};

	THREE.MZ3Loader.prototype = {

		constructor: THREE.MZ3Loader,

		load: function ( url, onLoad, onProgress, onError ) {
			var scope = this;
			var loader = new THREE.FileLoader( scope.manager );
			loader.setPath( scope.path );
			loader.setResponseType( 'arraybuffer' );
			loader.load( url, function ( arrayBuffer ) {
				onLoad( scope.parse( arrayBuffer ) );
			}, onProgress, onError );
		},

		setPath: function ( value ) {
			this.path = value;
			return this;
		},

		parse: function ( arrayBuffer ) {
			console.time( 'MZ3Loader' );
			var data = decodeMZ3( arrayBuffer );
			var bufferGeometry = new THREE.BufferGeometry();
			var attribute;
			if ( data.positions !== null )
				bufferGeometry.addAttribute( "position", new THREE.BufferAttribute( data.positions, 3, 0 ) );
			if ( data.indices !== null )
				bufferGeometry.setIndex( new THREE.BufferAttribute( data.indices, 1 ) );
			if ( data.colors !== null ) {
				//if ( data.colors.length > 0 )
				//console.log("has vertex colors!!!!");
				bufferGeometry.addAttribute( 'color', new THREE.Float32BufferAttribute( data.colors, 3 ) );
			}
			if ( data.uv2 !== null ) {
				//if ( data.colors.length > 0 )
				//console.log("has scalars");
				bufferGeometry.addAttribute( 'uv2', new THREE.Float32BufferAttribute( data.uv2, 2 ) );
				//bufferGeometry.addAttribute( 'color', new THREE.Float32BufferAttribute( data.colors, 3 ) );
			}
			//MNI space X/Y/Z is Left->Right/Posterior>Anterior/Foot>Head, for ThreeJS it is Left->Right,Low->High,Near->Far
			var matrix = new THREE.Matrix3().set(
				1, 0, 0,
				0, 0, 1,
				0, -1, 0
			);
			bufferGeometry.applyMatrix(matrix);
			//material = new THREE.MeshBasicMaterial({ vertexColors: THREE.VertexColors });
			bufferGeometry.computeVertexNormals();
			console.timeEnd( 'MZ3Loader' );
			return bufferGeometry;
		}
		//mesh.scale.x = 2

	};

} )( THREE );

THREE.MZ3Loader._loadScript = function ( src ) {
  var prevScript = document.getElementById( 'decoder_script' );
  if ( prevScript !== null ) {
    prevScript.parentNode.removeChild( prevScript );
  }
  var head = document.getElementsByTagName( 'head' )[ 0 ];
  var script = document.createElement( 'script' );
  script.id = 'decoder_script';
  script.type = 'text/javascript';
  script.src = src;
  return new Promise( function ( resolve ) {
    script.onload = resolve;
    head.appendChild( script );
  });
};
