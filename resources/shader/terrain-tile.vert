/*
 * Vertex shader for terrain tiles.
 */
#version 330 core

// Vertex attributes.
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;

// Premultiplied mvp matrix.
uniform mat4 mvpMatrix;

// The interpolated, but untransformed, vertex position.
out vec3 untransformedPosition;

void main()
{
  untransformedPosition = position;
  gl_Position = mvpMatrix * vec4(position, 1);
}
