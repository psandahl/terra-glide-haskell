/*
 * Vertex shader for terrain tiles.
 */
#version 330 core

// Vertex attributes.
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;

// Premultiplied mvp matrix.
uniform mat4 mvpMatrix;

// Premultiplied normal matrix which puts normals into view space.
uniform mat3 normalMatrix;

// The interpolated, but untransformed, vertex position.
out vec3 untransformedPosition;

// The transformed, but non normalized, normal.
out vec3 transformedNormal;

void main()
{
  untransformedPosition = position;
  transformedNormal = normalMatrix * normal;
  gl_Position = mvpMatrix * vec4(position, 1);
}
