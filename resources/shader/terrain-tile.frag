/*
 * Fragment shader for terrain tiles.
 */
#version 330 core

// The interpolated, but untransformed, vertex position.
in vec3 untransformedPosition;

// The transformed, but non normalized, normal.
in vec3 transformedNormal;

// The maximum height of the terrain.
uniform float terrainHeight;

// The terrain is shaded using two gradients, where each grandient have to
// colors. The colors are enumerated from lower terrains to upper.
uniform vec3 terrainColor0;
uniform vec3 terrainColor1;
uniform vec3 terrainColor2;
uniform vec3 terrainColor3;

// The ambient light; color and strength.
uniform vec3 ambientLightColor;
uniform float ambientLightStrength;

// The sun's color; transformed and normalized direction and color.
uniform vec3 transformedSunLightDirection;
uniform vec3 sunLightColor;

// The final color value.
out vec4 color;

vec3 terrainColor();
vec3 ambientLight();
vec3 sunLight();

void main()
{
  vec3 unfoggedColor = terrainColor() * (ambientLight() + sunLight());
  color = vec4(unfoggedColor, 1);
}

// Select the terrain color from the height of the fragment and the terrain
// color gradients.
vec3 terrainColor()
{
  float height = untransformedPosition.y / terrainHeight;

  vec3 color = mix(terrainColor0, terrainColor1, smoothstep(0.0, 0.20, height));
  color = mix(color, terrainColor2, smoothstep(0.20, 0.7, height));
  return mix(color, terrainColor3, smoothstep(0.7, 1.0, height));
}

// Calculate the ambient light.
vec3 ambientLight()
{
  return ambientLightColor * ambientLightStrength;
}

// Calculate the diffuse light from the sun.
vec3 sunLight()
{
  vec3 normal = normalize(transformedNormal);
  float diffuse = max(dot(normal, transformedSunLightDirection), 0);

  return sunLightColor * diffuse;
}
