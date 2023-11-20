import { Component, OnInit } from '@angular/core';
import { HelperPerfilesService } from './helperPerfiles.service';

@Component({
  selector: 'serv-talento-lazy-loaded-root',
  template: `<router-outlet></router-outlet>`,
  providers: [HelperPerfilesService],
})
export class LazyLoadedRootComponent implements OnInit {
  constructor() {}

  ngOnInit() {}
}
