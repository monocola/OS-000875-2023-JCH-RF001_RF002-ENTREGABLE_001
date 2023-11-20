import { Component } from '@angular/core';
import { Router } from '@angular/router';

@Component({
  selector: 'gme-web-not-found',
  styleUrls: ['./not-found.component.scss'],
  templateUrl: './not-found.component.html',
})
export class NotFoundComponent {
  constructor(private route: Router) {}

  goHome() {
    this.route.navigateByUrl('/pages');
  }
}
