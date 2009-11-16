---
layout: default
title: Statet plugin Eclipse - instalacja 
---

Instaluj� pakiet eclipse-sdk z wszystkimi zale�no�ciami np. na gentoo emerge eclipse-sdk. Nast�pnie z katalogu /usr/lib64/eclipse-3.6 usuwam zawarto�� i kopiuj� tam z [mirrora Eclipse] (http://www.mirrorservice.org/sites/download.eclipse.org/eclipseMirror/eclipse/downloads/drops/) wersj� eclipse-sdk 3.5 np. [dla linux 64-bit](http://www.mirrorservice.org/sites/download.eclipse.org/eclipseMirror/eclipse/downloads/drops/R-3.5.1-200909170800/eclipse-SDK-3.5.1-linux-gtk-x86_64.tar.gz). Po przekopiowaniu zawarto�ci pakietu do katalogu /usr/lib64/eclipse-3.6,umieszczam (je�li nie mam) skrypt eclipse-3.6 z UBI w /usr/local/bin i wykonuj� polecenie: 

eclipse-3.6 -clean

, kt�re czy�ci histori� z komputera, na kt�rym kompilowano eclipse-sdk.
Poprzez Help->Install New Software dopisuj� strony update plugin�w eclipse-sdk, najpierw [Egit](http://github.com/guides/using-the-egit-eclipse-plugin-with-github): http://www.jgit.org/updates, a potem [Statet](http://www.walware.de/goto/statet): http://download.walware.de/eclipse-3.5, po ka�dym dopisaniu zaznaczam biblioteki i narz�dzia w lewym oknie i wykonuj� Next, a nast�pnie update, czasami z zaznaczeniem licencji: I Agree.

W katalogu roboczym eclipse (domy�lnie /home/username/workspace) wykonuj�:

git clone git://github.com/pwasiewi/iso.git

Uzyskuj� katalog iso ze skryptami, nast�pnie uruchamiam New->R-Project i nazywam go tak samo jak poprzedni katalog czyli iso. Po przycisku Finish pokazuj� si� w folderze projektu iso sklonowane wcze�niej pliki. Jeszcze dodaj� do Windows->Preferences->StatET->Run/Debug->R-environment katalog "/usr/lib64/R" oraz "64bit". Po zdefiniowaniu w Run->Run Configurations->Rconsole new environment i uruchomieniu go uzyskuj� w konsoli na dole R-shell oraz mog� zaznaczaj�c tekst mysz� poprzez Ctrl-R Ctrl-R wykonywa� go w tej konsoli lub te� wykonywa� ca�e skrypty. Reszt� udogodnie� mo�na doczyta� w [manualu StatET](http://www.splusbook.com/Rintro/R_Eclipse_StatET.pdf).

Dodatkowe kursy j�zyka R na [stronie R cources](http://www.splusbook.com/Rintro/RCourseMaterial.html) oraz wst�p po polsku [w publikacji Komsty](http://cran.r-project.org/doc/contrib/Komsta-Wprowadzenie.pdf).

Na temat git [wolnodost�pna ksi��ka ProGit book](http://progit.org/book/) oraz filmy [Linus on Git](http://www.youtube.com/watch?v=4XpnKHJAok8) oraz [Git intro](http://video.linuxfoundation.org/video/1516), z tym, �e ten ostatni najlepiej w Youtube si� prezentuje i trzeba si� pogodzi� z za bardzo skompresowanym d�wi�kiem.

Dodatkowo polecam [opis wykonania strony na GitHubie](http://blog.envylabs.com/2009/08/publishing-a-blog-with-github-pages-and-jekyll/) z list� komend �ci�gaj�cych stron� www projektu po wykonaniu jej automatycznie przyciskiem na GitHubie:

git clone git://github.com/pwasiewi/iso.git
cd iso
git symbolic-ref HEAD refs/heads/gh-pages; rm .git/index; git clean -fdx; git pull origin gh-pages

Po tych komendach mo�na git checkout master lub git checkout gh-pages prze��cza� si� mi�dzy repozytoriami skrypt�w i strony projektu.

Strona g��wna u�ytkownika znajduje si� w repozytorium username.github.com, gdy� najpierw zak�adamy sobie konto, a potem twprzymy w�asne repozytoria np.: takie jakiso lub w�a�nie pwasiewi.github.com

Dla vim fanatyk�w polecam stron� z [Vim plugins.](http://www.catonmat.net/blog/vim-plugins-snipmate-vim/)

