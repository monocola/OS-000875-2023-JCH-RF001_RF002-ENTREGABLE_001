import { NbIconLibraries } from '@nebular/theme';
import { Component, OnInit } from '@angular/core';
import { Title } from '@angular/platform-browser';
import { filter, map } from 'rxjs/operators';
import { ActivatedRoute, NavigationEnd, Router } from '@angular/router';

@Component({
  selector: 'serv-talento-app',
  template: `
    <router-outlet></router-outlet>
    <ngx-spinner
      id="ngx-spinner"
      bdColor="rgba(255, 255, 255, 0.6)"
      size="default"
      color="#0d88bc"
      type="ball-square-spin"
      [fullScreen]="true"
    ></ngx-spinner>
  `,
})
export class AppComponent implements OnInit {
  constructor(
    private iconsLibrary: NbIconLibraries,
    private router: Router,
    private activatedRoute: ActivatedRoute,
    private titleService: Title
  ) {
    this.iconsLibrary.registerFontPack('entweb', { iconClassPrefix: 'ent-web' });
    this.iconsLibrary.setDefaultPack('entweb');
  }

  ngOnInit(): void {
    const appTitle = this.titleService.getTitle();
    this.router.events
      .pipe(
        filter((event) => event instanceof NavigationEnd),
        map(() => {
          let child = this.activatedRoute.firstChild;
          while (child.firstChild) {
            child = child.firstChild;
          }
          if (child.snapshot.data['title']) {
            return child.snapshot.data['title'];
          }
          return appTitle;
        })
      )
      .subscribe((ttl: string) => {
        this.titleService.setTitle(ttl);
      });
  }
}
