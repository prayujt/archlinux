#version 440
layout(location = 0) in vec2 qt_TexCoord0;
layout(location = 0) out vec4 fragColor;
layout(std140, binding = 0) uniform buf {
    mat4 qt_Matrix;
    float qt_Opacity;
    float phase;
    vec3 sunDirection;
};
layout(binding = 1) uniform sampler2D dayTexture;
layout(binding = 2) uniform sampler2D cloudTexture;
layout(binding = 3) uniform sampler2D nightTexture;
layout(binding = 4) uniform sampler2D dayMarkerTexture;
const float PI = 3.14159265359;
const float TILT = 0.4091;

float cityLight(vec2 uv) {
    vec3 lights = texture(nightTexture, uv).rgb;
    return smoothstep(0.06, 0.65, dot(lights, vec3(0.299, 0.587, 0.114)));
}
void main() {
    vec2 p = (qt_TexCoord0 - 0.5) / 0.475;
    float r = length(p);
    float edge = max(fwidth(r), 0.001);
    vec3 sun = normalize(sunDirection);
    // Transform sunlight into the view only for the thin atmospheric rim.
    vec3 sunAxis = vec3(sun.x * cos(phase) + sun.z * sin(phase), sun.y,
                       -sun.x * sin(phase) + sun.z * cos(phase));
    vec3 sunView = vec3(sunAxis.x * cos(TILT) + sunAxis.y * sin(TILT),
                       -sunAxis.x * sin(TILT) + sunAxis.y * cos(TILT), sunAxis.z);
    if (r >= 1.0) {
        float lit = 0.15 + 0.85 * max(0.0, dot(normalize(vec3(p.x, -p.y, 0.03)), sunView));
        float alpha = exp(-(r - 1.0) * 150.0) * 0.15 * lit * qt_Opacity;
        fragColor = vec4(vec3(0.82, 0.83, 0.79) * alpha, alpha);
        return;
    }
    vec3 normal = vec3(p.x, -p.y, sqrt(max(0.0, 1.0 - r * r)));
    vec3 axis = vec3(normal.x * cos(TILT) - normal.y * sin(TILT),
                     normal.x * sin(TILT) + normal.y * cos(TILT), normal.z);
    vec3 geographic = vec3(axis.x * cos(phase) - axis.z * sin(phase), axis.y,
                          axis.x * sin(phase) + axis.z * cos(phase));
    vec2 uv = vec2(fract(atan(geographic.x, geographic.z) / (2.0 * PI) + 0.5),
                   0.5 - asin(clamp(geographic.y, -1.0, 1.0)) / PI);
    vec3 sampleColor = texture(dayTexture, uv).rgb;
    float land = 1.0 - smoothstep(0.025, 0.075, sampleColor.b - sampleColor.r);
    // Lighting follows the actual UTC time and season, independent of the display spin.
    float incidence = dot(geographic, sun);
    float diffuse = pow(max(0.0, incidence), 0.7);
    float daylight = smoothstep(-0.12, 0.16, incidence);
    float night = 1.0 - smoothstep(-0.10, 0.04, incidence);
    float cloud = smoothstep(0.28, 0.9, texture(cloudTexture, uv).r) * 0.12;
    // Engraved marks follow the sphere instead of a flat grid over the screen.
    vec2 lattice = uv * vec2(720.0, 360.0);
    vec2 cell = fract(lattice) - 0.5;
    float dotRadius = 0.20 + 0.16 * max(land, cloud);
    vec2 footprint = fwidth(lattice);
    float aa = max(length(footprint) * 0.35, 0.01);
    float dots = 1.0 - smoothstep(dotRadius - aa, dotRadius + aa, length(cell));
    // Below two pixels per cell, blend to its average coverage instead of aliasing.
    float minified = smoothstep(0.30, 0.50, max(footprint.x, footprint.y));
    dots = mix(dots, PI * dotRadius * dotRadius, minified);
    float base = (0.025 + land * 0.13) * (0.35 + 0.65 * diffuse);
    float ink = (0.07 + land * 0.72 + cloud) * (0.15 + 0.85 * diffuse);
    float rim = pow(1.0 - normal.z, 4.5) * (0.03 + 0.15 * daylight);
    vec3 color = vec3(0.94, 0.95, 0.91) * (base + dots * ink + rim);
    // Settlements stay subtle gold by day and become brighter blue lights at night.
    float cities = cityLight(uv);
    vec2 texel = vec2(1.0 / 4096.0, 1.0 / 2048.0);
    float halo = (cityLight(uv + vec2(texel.x, 0.0)) + cityLight(uv - vec2(texel.x, 0.0))
                + cityLight(uv + vec2(0.0, texel.y)) + cityLight(uv - vec2(0.0, texel.y))) * 0.25;
    color += vec3(0.65, 0.84, 1.0) * pow(cities, 0.65) * night * 1.8;
    color += vec3(0.22, 0.46, 0.70) * halo * night * 0.65;
    float dayMarker = texture(dayMarkerTexture, uv).a;
    color += vec3(0.95, 0.85, 0.59) * dayMarker * (1.0 - night) * 0.42;
    float alpha = (1.0 - smoothstep(1.0 - edge * 1.5, 1.0, r)) * qt_Opacity;
    fragColor = vec4(color * alpha, alpha);
}
