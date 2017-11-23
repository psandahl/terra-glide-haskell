/*
 * Fragment shader for terrain tiles.
 */
#version 330 core

// The maximum height of the terrain (terrain might be transformed in x and z
// but never in y).
uniform float terrainHeight;

// The terrain is shaded using two gradients, where each grandient have to
// colors. The colors are enumerated from lower terrains to upper.
uniform vec3 terrainColor0;
uniform vec3 terrainColor1;
uniform vec3 terrainColor2;
uniform vec3 terrainColor3;

out vec4 color;

void main()
{
  color = vec4(terrainColor3, 1);
}
