module.exports = {
	globDirectory: '.',
	globPatterns: [
	  'site_libs/**/*.{js,css,woff,eot,ttf,woff2,asc,ico,json}',
		'images/favicons/*.{svg,png,webp}',
		'images/logo*.webp',
		//'pdf/publications/*.pdf',
		//'**/*.{js,css,woff,eot,ttf,woff2,webp,webm,svg,png,,pdf,asc,ico,json}'
		'**/*.{html}'
	],
  swSrc: '../ws-src.js',
	swDest: '../injectManifest.js'
};