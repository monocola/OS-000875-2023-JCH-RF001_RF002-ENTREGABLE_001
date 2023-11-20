import { Component } from '@angular/core';

@Component({
  selector: 'serv-talento-pages',
  styleUrls: ['pages.component.scss'],
  template: `
    <nb-layout>
      <nb-layout-header fixed class="mat-elevation-z3">
        <serv-talento-header class="dashboardHeader"></serv-talento-header>
      </nb-layout-header>

      <nb-sidebar class="dashboardSidebar">
        <serv-talento-sidenav></serv-talento-sidenav>
      </nb-sidebar>

      <nb-layout-column class="rootBase">
        <div class="body__wide">
          <router-outlet></router-outlet>
        </div>
        <div class="body__footer">
          <serv-talento-footer></serv-talento-footer>
        </div>
      </nb-layout-column>
    </nb-layout>
  `,
})
export class PagesComponent {}
