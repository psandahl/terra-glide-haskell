/*
 * Water fragment shader.
 */
#version 330 core

// The interpolated clip space coordinate for the fragment.
in vec4 clipSpace;

// The water color.
uniform vec3 waterColor;

// The refraction texture.
uniform sampler2D refractionTexture;

// The resulting output color.
out vec4 color;

void main()
{
  // Make a texture coordinate/normalized device coordinate.
  vec2 texCoord = (clipSpace.xy / clipSpace.w) / 2 + 0.5;

  // Mix the water color with the refraction texture.
  color = vec4(mix(texture2D(refractionTexture, texCoord).rgb, waterColor, 0.50), 1);
}
