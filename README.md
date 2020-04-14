# My blog
The repository contains my personal blog code. The blog is built by Pelican static site generator. It is used pelican-bootstrap3 theme.

The included plugins are 
1. tipue_search.
2. more_categories.
3. minify.

The changes to pelican-bootstrap3 theme are
1. Modified article_info.html template to include more_categories implementation.
2. Modified article_list.html template and style.css to line up vertically artile image and header.
3. Mofified article_list.html template to add `alt` attribute to article logo images. SEO requests that all images have to have `alt` attribute. Logo images should be located in a folder, for example, **extra** and names should include `-` character to separate words. `Cover: /extra/synology-logo.png` is converted to `alt="Synology logo"`.
4. Modified base.html template to add Meta Description Tag. It's mandatory SEO element. **pelicanconf.py** file should have `SITE_DESCRIPTION` variable, for example `SITE_DESCRIPTION = 'A personal blog.'`. 
5. Modified base.html template to add `alt` attribute to site logo image. The value is `SITENAME`. SEO requests that all images have to have `alt` attribute.

The actual blog is located [here](https://techjogging.com).