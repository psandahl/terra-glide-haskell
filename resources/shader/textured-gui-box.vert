/*
 * Vertex shader for the textured gui box.
 */
#version 330 core

// Vertex attributes.
layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoord;

uniform mat4 modelMatrix;

// Interpolate the texture coordinates.
out vec2 vTexCoord;

void main()
{
  vTexCoord = texCoord;
  gl_Position = modelMatrix * vec4(position, 1);
}
