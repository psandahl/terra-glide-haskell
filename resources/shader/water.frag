/*
 * Water fragment shader.
 */
#version 330 core

// The interpolated clip space coordinate for the fragment.
in vec4 clipSpace;

// Texture coordinates for the DuDv map.
in vec2 dudvCoord;

// The water color.
uniform vec3 waterColor;

// The refraction texture.
uniform sampler2D refractionTexture;

// The reflection texture.
uniform sampler2D reflectionTexture;

// The DuDv texture.
uniform sampler2D dudvTexture;

uniform float waveMove;

// The resulting output color.
out vec4 color;

const float waveLength = 0.015;

void main()
{
  // Make a texture coordinate/normalized device coordinate.
  vec2 texCoord = (clipSpace.xy / clipSpace.w) / 2 + 0.5;

  // Pick the DuDv distortion value.
  vec2 dist1 =
    (texture2D(dudvTexture, vec2(dudvCoord.x + waveMove, dudvCoord.y)).rg * 2 - 1) * waveLength;
  vec2 dist2 =
    (texture2D(dudvTexture, vec2(-dudvCoord.x + waveMove, dudvCoord.y + waveMove)).rg * 2 - 1) * waveLength;
  vec2 dist = dist1 + dist2;

  vec2 refractionCoord = texCoord + dist;
  vec2 reflectionCoord = vec2(texCoord.s, 1 - texCoord.t) + dist;

  // Clamp the texture coordinates.
  refractionCoord = clamp(refractionCoord, 0.001, 0.999);
  reflectionCoord = clamp(reflectionCoord, 0.001, 0.999);

  // Pick the refraction color.
  vec3 refraction = texture2D(refractionTexture, refractionCoord).rgb;

  // Pick the reflection color.
  vec3 reflection = texture2D(reflectionTexture, reflectionCoord).rgb;

  // Mix the water color with the refraction texture.
  color = vec4(mix(refraction, reflection, 0.5), 1);
  //color = vec4(dudv, 1);
}
