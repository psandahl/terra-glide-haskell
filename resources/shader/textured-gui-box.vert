/*
 * Vertex shader for the textured gui box.
 */
#version 330 core

// Vertex attributes.
layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoord;

// Interpolate the texture coordinates.
out vec2 vTexCoord;

void main()
{
  vTexCoord = texCoord;
  gl_Position = vec4(position, 1);
}
