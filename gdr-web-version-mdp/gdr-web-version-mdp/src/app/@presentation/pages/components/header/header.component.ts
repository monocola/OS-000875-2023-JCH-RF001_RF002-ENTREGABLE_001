import { Component, OnInit, ViewChild } from '@angular/core';
import { faSignOutAlt } from '@fortawesome/free-solid-svg-icons';
import { NbSidebarService } from '@nebular/theme';
import { SidenavService } from 'src/app/@data/services/sidenav.service';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { Const } from '../../../../@data/services/const';
import { forkJoin } from 'rxjs';
import { ToastService } from '../../../@common-components/toast';
import { Router } from '@angular/router';
import { ImplementacionRepository } from '../../../../@domain/repository/implementacion.repository';

@Component({
  selector: 'serv-talento-header',
  templateUrl: './header.component.html',
  styleUrls: ['./header.component.scss'],
})
export class HeaderComponent implements OnInit {
  @ViewChild('ocultarEspacio') ocultarEspacio;

  url =
    'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcT3iyWiS5Tu20L5S91HBmKnuRTNT8DffM8MCg&usqp=CAU';
  profile = this.authenticationService.getCurrentUserValue;
  iconOut = faSignOutAlt;
  nombreRol = '';
  entidad;
  imgEntidad = './assets/images/logo.png';
  roles;
  rolesByEntidad;
  rolId;
  rol;

  constructor(
    private authenticationService: AuthenticationRepository,
    public implementacionService: ImplementacionRepository,
    public sidebarService: NbSidebarService,
    public sidenavService: SidenavService,
    private toast: ToastService,
    private router: Router
  ) {}

  ngOnInit(): void {
    setTimeout(() => {
      this.subscribeEvents();
      this.rol = JSON.parse(sessionStorage.getItem('roles'));
      this.entidad = JSON.parse(sessionStorage.getItem('entidad'));
      if ( this.entidad !== null  && this.entidad.logo !== null ) {
        this.imgEntidad = Const.API_FILE_SERVER + this.entidad.logo;
      }
      this.nombreRol = this.rol.nombreRol;
      this.loadCombox();
    }, 0);

  }

  logout() {
    this.authenticationService.logout();
  }

  clickBar() {
    !this.sidenavService.collapsed
      ? this.sidebarService.compact()
      : this.sidebarService.expand();
  }

  subscribeEvents() {
    this.sidebarService.onCompact().subscribe(() => {
      this.sidenavService.collapsed = true;
    });
    this.sidebarService.onExpand().subscribe(() => {
      this.sidenavService.collapsed = false;
    });
  }

  loadCombox() {
    const roles = this.authenticationService.getRolEntidad(this.entidad.entidadId, null);
    forkJoin([roles]).subscribe(
      (results ) => {
        this.rolesByEntidad = results[0];
        if (this.rolesByEntidad.length !== 0 && this.rolesByEntidad.length !== undefined ) {
          this.roles = this.rolesByEntidad.filter( item => item.rolId !== this.rol.rolId );
        } else {
          this.ocultarEspacio.nativeElement.style.visibility = "hidden";
        }
      });
  }

  cambioRol(rolId, rol ) {
    console.log("rol:",rol)
    if (!this.implementacionService.validateTempMenu()) {
      sessionStorage.removeItem('roles');
      this.getObtenerRoles(this.entidad.entidadId, rolId, rol.nombreRol);
    }
  }

  getObtenerRoles(entidadId, rolId,nombreRolId?:any) {
    this.rolId = rolId;
    if (this.rolId !== null) {
      this.authenticationService.getRolEntidad(entidadId, this.rolId,nombreRolId).subscribe(
        item => {

          console.log("item menu:",item)
          if ( item.length === 1 ) {
            this.verifyEntityByUpdatedRol(entidadId, this.rolId);
          } else {
            this.verifyEntityByUpdatedRol(entidadId, this.rolId);
          }
        });
    }
  }
 
  verifyEntityByUpdatedRol(entidadId, idRol) {
    this.authenticationService
      .verifyEntityRolUpdated(
        entidadId, idRol
      )
      .subscribe(
        (res) => {
          if (res === '0') {
            this.router.navigateByUrl('/pages/home');
          } else {
            this.router.navigateByUrl('/pages');
          }
          this.cambiaRolPersona(JSON.parse(sessionStorage.getItem('persona')), idRol);
          this.ngOnInit();
        },
        (err) => {
          this.authenticationService.clearUser();
          this.toast.showToast(err.message, 'danger');
        }
      );
  }

  cambiaRolPersona(persona, idRol) {
    let personaRol = persona;
    sessionStorage.removeItem('persona');
    personaRol.rolId = idRol;
    sessionStorage.setItem('persona', JSON.stringify(personaRol));
    this.profile = this.authenticationService.getCurrentUserValue;
  }

}
