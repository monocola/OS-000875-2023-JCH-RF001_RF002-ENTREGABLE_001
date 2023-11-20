import { Component, OnInit } from '@angular/core';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { User } from 'src/app/@data/model/user';

@Component({
  selector: 'serv-talento-default-page',
  templateUrl: './default-page.component.html',
  styleUrls: ['./default-page.component.scss']
})

export class DefaultPageComponent implements OnInit {

  user: User;

  constructor(
    private authRepository: AuthenticationRepository
  ) { }

  ngOnInit(): void {
    this.user = this.authRepository.getCurrentUserValue;
  }

}
