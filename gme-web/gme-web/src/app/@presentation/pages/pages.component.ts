import { Component } from '@angular/core';

@Component({
  selector: 'gme-web-pages',
  styleUrls: ['pages.component.scss'],
  template: `
    <nb-layout>
      <nb-layout-header fixed class="mat-elevation-z3">
        <gme-web-header class="dashboardHeader"></gme-web-header>
      </nb-layout-header>

      <nb-sidebar class="dashboardSidebar">
        <gme-web-sidenav></gme-web-sidenav>
      </nb-sidebar>

      <nb-layout-column class="rootBase">
        <div class="body__wide">
          <router-outlet></router-outlet>
        </div>
        <div class="body__footer">
          <gme-web-footer></gme-web-footer>
        </div>
      </nb-layout-column>
    </nb-layout>
  `,
})
export class PagesComponent {}
