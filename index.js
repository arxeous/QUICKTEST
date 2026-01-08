import * as Three from "three";
import {OrbitControls} from "jsm/controls/OrbitControls.js";


function Rand(a, b)
{
    return a + Three.MathUtils.randInt(0, 32767) % ((b-a) + 1);
}

function RandF(a, b)
{
    return a + Three.MathUtils.randFloat(0,1) * (b-a);
}
// Window set up
const w = window.innerWidth;
const h = window.innerHeight;
const renderer = new Three.WebGLRenderer({antialias: true});
renderer.setSize(w, h);
document.body.appendChild(renderer.domElement);

const n = 256;
const camera = new Three.PerspectiveCamera()
camera.position.set(0,-15,5);  // Move back to see the plane
camera.lookAt(0, 0, 0);

const controls = new OrbitControls(camera, renderer.domElement);
const scene = new Three.Scene();


// Wave simulation.
// Timer will be exclusively used for wave disturbances in replacement for our game timer.
const timer = new Three.Timer();
timer.connect( document );  // no idea what this actually does but its in the docs!
let t_base = 0;

 // Wave constant values
const dampVal = 0.2;
const dt = 0.03;
const dx = 0.25;
const d = dampVal * dt + 2.0;
const speed = 1.5;
const e = (speed * speed) * (dt * dt) / (dx*dx);

const mK1 = (dampVal * dt - 2.0) / d;
const mK2 = (4.0 - 8.0 * e) / d;
const mK3 = (2.0 * e) / d;

let prevBuffer = new Three.WebGLRenderTarget(n, n,
    {
        minFilter: Three.NearestFilter,
        magFilter: Three.NearestFilter,
        format: Three.RGBAFormat,
        type: Three.FloatType
    }
);
let currBuffer = prevBuffer.clone();
let nextBuffer = prevBuffer.clone();

// Shader creation in three.js. very strange.
const computeWaves = new Three.ShaderMaterial(
    {
        // These act as our constant buffers
        uniforms: {
        gCurrSolInput: { value: null },
        gPrevSolInput: { value: null },
        texelSize: { value: 1.0 / n },
        damping: { value: dampVal },
        mk1: {value: mK1},
        mk2: {value: mK2},
        mk3: {value: mK3}
  },
  vertexShader:
  `
    varying vec2 vUv;
    void main() {
        vUv = uv;
        gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
    }
  `,
  fragmentShader:
  `
    uniform sampler2D gCurrSolInput;
    uniform sampler2D gPrevSolInput;
    uniform float texelSize;
    uniform float damping;
    uniform float mk1;
    uniform float mk2;
    uniform float mk3;
    varying vec2 vUv;
    
    void main() {
      float current = texture2D(gCurrSolInput, vUv).r;
      float prev = texture2D(gPrevSolInput, vUv).r;
      
      float left = texture2D(gCurrSolInput, vUv + vec2(-texelSize, 0.0)).r;
      float right = texture2D(gCurrSolInput, vUv + vec2(texelSize, 0.0)).r;
      float up = texture2D(gCurrSolInput, vUv + vec2(0.0, texelSize)).r;
      float down = texture2D(gCurrSolInput, vUv + vec2(0.0, -texelSize)).r;
      
      float waveCalc = mk1 * prev + mk2 * current + mk3 * (left + right + up + down);
      
      gl_FragColor = vec4(waveCalc, 0.0, 0.0, 1.0);
    }
  `
    }
);


const simulationQuad = new Three.Mesh(
  new Three.PlaneGeometry(2, 2),
  computeWaves
);
const simulationScene = new Three.Scene();
simulationScene.add(simulationQuad);
const simulationCamera = new Three.OrthographicCamera(-1, 1, 1, -1, 0, 1);

function disturbWaves(i, j, mag)
{
    const u = i / n;
    const v = j / n;

    const disturb = new Three.ShaderMaterial(
        {
            uniforms: 
            {
                gCurrSolInput: {value: currBuffer.texture},
                gDisturbIndex: {value: new Three.Vector2(u,v)},
                gDisturbMag: {value: mag}
            },
            vertexShader:
            `
                varying vec2 vUv;
                void main() 
                {
                    vUv = uv;
                    gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0f);
                } 
            `,
            fragmentShader:
            `
                uniform sampler2D gCurrSolInput;
                uniform vec2 gDisturbIndex;
                uniform float gDisturbMag;
                varying vec2 vUv;

                void main()
                {
                    float halfMag = 0.5f * gDisturbMag;
                    float curr = texture2D(gCurrSolInput, vUv).r;

                    float dist = distance(vUv, gDisturbIndex);
                    if (dist < 0.001f)
                    {
                        curr += gDisturbMag;
                    }
                    else if(dist < 0.008f)
                    {
                        curr += halfMag;
                    }
                    
                    gl_FragColor = vec4(curr, 0.0f, 0.0f, 1.0f);
                }
            `
        }
    );

    const quad = new Three.Mesh(
        new Three.PlaneGeometry(2, 2), 
        disturb);
    const tempScene = new Three.Scene();
    tempScene.add(quad);
  
    renderer.setRenderTarget(nextBuffer);
    renderer.render(tempScene, simulationCamera);
    renderer.setRenderTarget(null);

    let temp = currBuffer;
    currBuffer = nextBuffer;
    nextBuffer = temp;
}

function updateWaves() {
    timer.update();
    t_base += timer.getDelta();
    if(t_base > 0.25)
    {
        t_base = 0;
        let i = Rand(4, n - 5);
        let j = Rand(4, n - 5);

        let r = RandF(0.1, 0.2);

        disturbWaves(i,j,r);
    }
  
  
    computeWaves.uniforms.gCurrSolInput.value = currBuffer.texture;
    computeWaves.uniforms.gPrevSolInput.value = prevBuffer.texture;
  
    renderer.setRenderTarget(nextBuffer);
    renderer.render(simulationScene, simulationCamera);
    renderer.setRenderTarget(null);
  
    // Swap buffers
    let temp = prevBuffer;
    prevBuffer = currBuffer;
    currBuffer = nextBuffer;
    nextBuffer = temp;
}

//

const Light =
{
  value: {
        Strength: new Three.Vector3(0.9, 0.9, 0.9),
        Direction: new Three.Vector3(1, 1, -1.5),
    }
}

const Material = 
{
  value :
  {
    DiffuseAlbedo: new Three.Vector4(0.0, 0.2, 0.6, 0.5),
    FresnelR0: new Three.Vector3(0.2, 0.2, 0.2),
    Shininess: 1.0
  }
}

const displayMaterial = new Three.ShaderMaterial({
  transparent: true,
  uniforms: {
    heightMap: { value: null },
    gLight: Light,
    gMat: Material,
    gEyePosW: {value: new Three.Vector3(1.0, 1.0, 1.0)},
    gAmbientLight: {value: new Three.Vector4(0.0, 0.25, 0.35, 1.0)},
    gWorld: {value: null}
  },
  vertexShader: `
    uniform sampler2D heightMap;
    uniform mat3 gWorld;
    varying vec3 normalW;
    varying vec3 positionW;
    
    void main() {
      // Sample height from simulation texture
      float height = texture2D(heightMap, uv).r;
      vec3 displaced = position + normal * height;
      
      gl_Position = projectionMatrix * modelViewMatrix * vec4(displaced, 1.0);
      normalW = normal * gWorld;
      positionW = position * gWorld;
    }
  `,
  fragmentShader: `
    struct Light
    {
      vec3  Strength;
      vec3  Direction;   // directional/spot light only
    };
    struct Material
    {
      vec4 DiffuseAlbedo;
      vec3 FresnelR0;
      float Shininess;
    };

    uniform Light gLight;
    uniform vec3 gEyePosW;
    uniform Material gMat;
    uniform vec4 gAmbientLight;
    varying vec3 normalW;
    varying vec3 positionW;

    // Schlick gives an approximation to Fresnel reflectance (see pg. 233 "Real-Time Rendering 3rd Ed.").
    // R0 = ( (n-1)/(n+1) )^2, where n is the index of refraction.
    vec3 SchlickFresnel(vec3 R0, vec3 normal, vec3 lightVec)
    {
      float cosIncidentAngle = clamp(dot(normal, lightVec), 0.0, 1.0);

      float f0 = 1.0f - cosIncidentAngle;
      vec3 reflectPercent = R0 + (1.0f - R0)*(f0*f0*f0*f0*f0);

      return reflectPercent;
    }

    vec3 BlinnPhong(vec3 lightStrength, vec3 lightVec, vec3 normal, vec3 toEye, Material mat)
    {
      float m = mat.Shininess * 256.0f;
      vec3 halfVec = normalize(toEye + lightVec);

      float roughnessFactor = (m + 8.0f)*pow(max(dot(halfVec, normal), 0.0f), m) / 8.0f;
      vec3 fresnelFactor = SchlickFresnel(mat.FresnelR0, halfVec, lightVec);

      vec3 specAlbedo = fresnelFactor*roughnessFactor;

      // Our spec formula goes outside [0,1] range, but we are 
      // doing LDR rendering.  So scale it down a bit.
      specAlbedo = specAlbedo / (specAlbedo + 1.0f);

      return (mat.DiffuseAlbedo.rgb + specAlbedo) * lightStrength;
    }

    vec3 ComputeDirectionalLight(Light L, Material mat, vec3 normal, vec3 toEye)
    {
      // The light vector aims opposite the direction the light rays travel.
      vec3 lightVec = -L.Direction;

      // Scale light down by Lambert's cosine law.
      float ndotl = max(dot(lightVec, normal), 0.0f);
      vec3 lightStrength = L.Strength * ndotl;

      return BlinnPhong(lightStrength, lightVec, normal, toEye, mat);
    }

    vec4 ComputeLighting(Light gLight, Material mat, vec3 pos, vec3 normal, vec3 toEye, vec3 shadowFactor)
    {
      vec3 result = vec3(0.0f, 0.0f, 0.0f);
      result += shadowFactor[0] * ComputeDirectionalLight(gLight, mat, normal, toEye);

      return vec4(result, 0.0f);
    }

    void main() 
    {
      // Interpolating normal can unnormalize it, so renormalize it.
      vec3 temp = normalize(normalW);

      // Vector from point being lit to our eye. 
	    vec3 toEyeW = gEyePosW - positionW;
	    float distToEye = length(toEyeW);
	    toEyeW /= distToEye;

      // Light terms.
      vec4 ambient = gAmbientLight* gMat.DiffuseAlbedo;
      vec3 shadowFactor = vec3(1.0f, 1.0f, 1.0f);
      vec4 directLight = ComputeLighting(gLight, gMat, positionW,
      temp, toEyeW, shadowFactor);
      vec4 litColor = ambient + directLight;
      // Traditionally you take whatever alpha is in our diffuse albedo
      litColor.a = gMat.DiffuseAlbedo.a;

      gl_FragColor = litColor;
    }
  `
});
const worldMatrix = new Three.Matrix3();

const mesh = new Three.Mesh(
  new Three.PlaneGeometry(10, 10, n, n),
  displayMaterial
);

scene.add(mesh)
// In render loop
function animate() {
    requestAnimationFrame(animate);
    updateWaves();
    displayMaterial.uniforms.heightMap.value = nextBuffer.texture;
    camera.updateMatrixWorld();
    let camPos = camera.position.clone();
    camPos.applyMatrix3(camera.matrixWorld);
    displayMaterial.uniforms.gEyePosW.value = camPos;
    mesh.updateMatrixWorld();
    worldMatrix.getNormalMatrix(mesh.matrixWorld);
    displayMaterial.uniforms.gWorld = {value: worldMatrix};
    renderer.render(scene, camera);
}
animate();

