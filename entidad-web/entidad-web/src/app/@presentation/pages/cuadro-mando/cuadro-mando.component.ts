import { Component, OnInit } from '@angular/core';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { Const } from 'src/app/@data/services/const';
import { Router } from '@angular/router';

@Component({
  selector: 'serv-talento-cuadro-mando',
  templateUrl: './cuadro-mando.component.html',
  styleUrls: ['./cuadro-mando.component.scss'],
})
export class CuadroMandoComponent implements OnInit {
  roles = [];
  rol = 0;
  cont = 0;
  constructor(
    private authenticationRepository: AuthenticationRepository,
    public router: Router
  ) {}

  ngOnInit(): void {
    this.roles = JSON.parse(sessionStorage.getItem('userRoles'));

    this.roles.forEach((e) => {
      if (e.rolId === +Const.R_GESTOR_ORH) {
        this.cont++;
        this.rol = +Const.R_GESTOR_ORH;
      }

      if (e.rolId === +Const.R_COORDINADOR) {
        this.cont++;
        this.rol = +Const.R_COORDINADOR;
      }

      if (e.rolId === +Const.R_ADMIN_SERVIR) {
        this.cont++;
        this.rol = +Const.R_ADMIN_SERVIR;
      }
      if (e.rolId === +Const.R_ADMIN_ENTIDAD) {
        this.cont++;
        this.rol = +Const.R_ADMIN_ENTIDAD;
      }
    });
    if (this.cont === 1) {
      if (this.rol === +Const.R_GESTOR_ORH) {
        this.router.navigateByUrl('pages/cuadromando/gestor');
      } else if (this.rol === +Const.R_COORDINADOR) {
        this.router.navigateByUrl('pages/cuadromando/coordinador');
      } else if (this.rol === +Const.R_ADMIN_SERVIR) {
        this.router.navigateByUrl('pages/cuadromando/servir');
      } else if (this.rol === +Const.R_ADMIN_ENTIDAD) {
        this.router.navigateByUrl('pages/cuadromando/admin');
      }
    }
  }
}
