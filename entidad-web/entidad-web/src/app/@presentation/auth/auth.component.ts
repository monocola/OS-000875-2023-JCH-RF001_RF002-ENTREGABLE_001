import { AfterViewInit, Component } from '@angular/core';
@Component({
  selector: 'serv-talento-pages',
  styleUrls: ['auth.component.scss'],
  template: `
    <nb-layout>
      <nb-layout-column class="flex-v-center layout" id="panel">
        <router-outlet></router-outlet>
        <img src="assets/images/logo-servir.svg" />
      </nb-layout-column>
    </nb-layout>
  `,
})
export class AuthComponent implements AfterViewInit {
  ngAfterViewInit(): void {
    document.getElementById(
      'panel'
    ).style.background = `url("assets/images/bg.png")`;
  }
}
