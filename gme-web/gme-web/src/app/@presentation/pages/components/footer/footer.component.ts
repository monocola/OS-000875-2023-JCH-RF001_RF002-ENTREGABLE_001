import { Component, OnInit } from '@angular/core';
import {
  faFacebook,
  faTwitter,
  faYoutube,
  faLinkedin,
} from '@fortawesome/free-brands-svg-icons';

@Component({
  selector: 'gme-web-footer',
  templateUrl: './footer.component.html',
  styleUrls: ['./footer.component.scss'],
})
export class FooterComponent implements OnInit {
  faFacebook = faFacebook;
  faTwitter = faTwitter;
  faYoutube = faYoutube;
  faLinkedin = faLinkedin;

  constructor() {}

  ngOnInit(): void {}
}
