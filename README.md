[![Netlify Status](https://api.netlify.com/api/v1/badges/95286b11-d57b-4e5b-89d2-ec4ed92cdd84/deploy-status)](https://app.netlify.com/sites/dfolio-website/deploys)

# dfolio website
[dfolio](https://dfolio.fr)' personal website build with [Quarto](https://quarto.org/).

## ToDo

### Dark mode style

See [Bootstrap Color modes](https://getbootstrap.com/docs/5.3/customize/color-modes/)

fix inline code contrast.


# Tools

## quarto-pub

To publish on quarto-pub

```{terminal}
quarto publish quarto-pub  --profile quarto-pub
```

## Watermark

To add a watermark to a batch of images:

```{terminal}
magick mogrify  -path m -background transparent -fill grey -font Calibri -size 140x80 -pointsize 14 -gravity southeast -annotate +0+0 '© 2024, D. Folio, https://dfolio.fr' *.webp
```

## Service worker

```{terminal}
cd _site
npx workbox generateSW ../workbox-config.js
```
