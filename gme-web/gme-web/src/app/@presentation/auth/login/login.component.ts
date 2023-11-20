import { AfterViewInit, Component, OnInit } from '@angular/core';
import { Validators, FormBuilder, FormGroup } from '@angular/forms';
import { Router } from '@angular/router';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { getHash } from 'src/app/utils/general';
import { EntidadComponent } from '../entidad/entidad.component';
import { PerfilComponent } from '../perfil/perfil.component';
import { MatDialog } from '@angular/material/dialog';
import { distinctUntilChanged } from 'rxjs/operators';
declare var $: any;

@Component({
  selector: 'gme-web-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.scss'],
})
export class LoginComponent implements OnInit, AfterViewInit {
  showPassword: boolean = false;
  logging = false;
  entidad: any[] = [];
  perfil: any[] = [];
  entidadId: number;
  perfilId: number;
  rolId: number;
  entidadesIds: string = null;
  token: string = null;
  loginForm: FormGroup;

  get f() {
    return this.loginForm.controls;
  }

  constructor(
    private formBuilder: FormBuilder,
    private authenticationService: AuthenticationRepository,
    private toast: ToastService,
    private router: Router,
    private dialog: MatDialog
  ) {}

  ngAfterViewInit(): void {
    setTimeout(() => {
      this.inicializarCaptcha();
    }, 1000);
  }

  ngOnInit(): void {
    this.loginForm = this.formBuilder.group({
      documentNumber: [
        '',
        [
          Validators.required,
          Validators.minLength(8),
          Validators.maxLength(12)
        ],
      ],
      password: [
        '',
        [
          Validators.required,
          Validators.minLength(4),
          Validators.maxLength(30),
        ],
      ],
      inputCaptcha: ['', [Validators.required, Validators.minLength(5)]],
    });

    this.loginForm.controls['documentNumber'].valueChanges
      .pipe(distinctUntilChanged())
      .subscribe((newValue: string) => {
        if (newValue.length === 8) {
          this.loginForm.controls.documentNumber.setValidators([
            Validators.required,
            Validators.minLength(8),
            Validators.maxLength(12),
            Validators.pattern(/^-?(|[0-9]\d*)?$/),
          ]);
        } else if (newValue.length <= 7 || newValue.length > 8) {
          this.loginForm.controls.documentNumber.setValidators([
            Validators.required,
            Validators.minLength(8),
            Validators.maxLength(12),
          ]);
        }

        this.loginForm.controls.documentNumber.markAsTouched();
        this.loginForm.controls.documentNumber.updateValueAndValidity();
      });
  }

  toggleShowPassword() {
    this.showPassword = !this.showPassword;
  }

  /*login() {
    if (this.loginForm.valid && !this.logging) {
      this.logging = true;
      const valorCaptcha = $('#captchaLogin').realperson('getHash');
      if (valorCaptcha === getHash(this.loginForm.get('inputCaptcha').value)) {
        const userForm = this.loginForm.get('documentNumber').value;
        const passwordForm = this.loginForm.get('password').value;
        this.authenticationService.login(userForm, passwordForm).subscribe(
          (res) => {
            this.logging = false;
            const user = this.authenticationService.getCurrentUserValue;
            if (user.rolId === Const.R_ADMIN_SERVIR) {
              this.router.navigateByUrl('/pages');
            } else {
              this.verifyEntityUpdated();
            }
          },
          (err) => {
            this.logging = false;
            this.authenticationService.clearUser();
            this.toast.showToast(err.message, 'danger');
          }
        );
      } else {
        this.logging = false;
        this.loginForm.get('inputCaptcha').setValue('');
        this.toast.showToast('El valor del captcha es incorrecto', 'danger');
      }
    }
  }*/

  verifyEntityUpdated() {
    this.authenticationService
      .verifyEntityUpdated(
        this.authenticationService.getCurrentUserValue.entidadId
      )
      .subscribe(
        (res) => {
          if (res === '0') {
              this.router.navigateByUrl('/pages/home');
          } else {
            this.router.navigateByUrl('/pages');
          }
        },
        (err) => {
          this.authenticationService.clearUser();
          this.toast.showToast(err.message, 'danger');
        }
      );
  }

  inicializarCaptcha(): void {
    $('#captchaLogin').realperson({ length: 5 });

    $('#captchaLogin').click(() => {
      const disable = $(this).text() === 'Disable';
      $(this).text(disable ? 'Enable' : 'Disable');
      $('#captchaLogin').realperson(disable ? 'disable' : 'enable');
    });

    $('#captchaLogin').click(() => {
      const destroy = $(this).text() === 'Remove';
      $(this).text(destroy ? 'Re-attach' : 'Remove');
      $('#captchaLogin').realperson(destroy ? 'destroy' : {});
    });
  }


/*
  getObtenerEntidad(resp) {
    this.rolId = null;
    let val = '';
    return this.authenticationService.getEntidad(resp.token).subscribe(
      item => {
        if (item.payload.items.length > 1) {
          item.payload.items.forEach( it => {
            val += it.entidadId + ',';
          });
          this.entidadesIds = val.slice(0, val.length - 1);
          const listEntidad = this.authenticationService.getListaEntidades(this.entidadesIds);
          return listEntidad.subscribe( resLst => {
            this.openModalEntidad(resLst.payload.entidad, this.rolId);
          });
        } else {
          if (item.payload.items.length === 1 ) {
            this.getObtenerRoles(item.payload.items[0].entidadId, this.rolId);
          }
        }
      });
  }*/

  /****************NEW LOGIN ************/
  login3() {
    let listaEntidad = null;
    this.rolId = null;
    if (this.loginForm.valid && !this.logging) {
      this.logging = true;
      const valorCaptcha = $('#captchaLogin').realperson('getHash');
      if (valorCaptcha === getHash(this.loginForm.get('inputCaptcha').value)) {
        const userForm = this.loginForm.get('documentNumber').value;
        const passwordForm = this.loginForm.get('password').value;
        this.authenticationService.login3(userForm, passwordForm).subscribe(
          (res) => {
            console.log(res);
            this.token = res.body.payload.accessToken;
            if (this.token) {
              listaEntidad = this.authenticationService.getEntidades(this.token).subscribe(
                item => {
                  listaEntidad = item;
                  this.authenticationService.getListaEntidades(listaEntidad).subscribe(
                    lista => {
                      if ( lista.payload.entidad.length > 1 ) {
                        this.openModalEntidad(lista.payload.entidad, this.rolId);
                      } else {
                        if ( lista.payload.entidad[0].entidadId !== null ) {
                          this.getObtenerRoles(lista.payload.entidad[0].entidadId, this.rolId);
                        } else {
                          throw new Error('El usuario no tiene acceso al sistema');
                        }
                      }
                    }
                  );
                }
              );
            } else {
              this.logging = false;
              this.loginForm.get('inputCaptcha').setValue('');
              this.toast.showToast('Error al iniciar Session', 'danger');
            }
          },
          (error) => {
            this.logging = false;
            this.authenticationService.clearUser();
            this.toast.showToast(error.message, 'danger');
          }
        );
      } else {
        this.logging = false;
        this.loginForm.get('inputCaptcha').setValue('');
        this.toast.showToast('El valor del captcha es incorrecto', 'danger');
      }
    }
  }

  getObtenerRoles(entidadId, rolId) {
    this.rolId = rolId;
    if (this.rolId !== null) {
      this.authenticationService.getRolEntidad(entidadId, this.rolId).subscribe(
        (item) => {
          if ( item.length === 1 ) {
            this.verifyEntityByUpdatedRol(entidadId, this.rolId);
          } else {
            this.verifyEntityByUpdatedRol(entidadId, this.rolId);
          }
        });
    } else {
      this.authenticationService.getRolEntidad(entidadId, this.rolId).subscribe(
        (item) => {
          if ( item.length > 1 ) {
            this.openModalPerfil(item, entidadId);
          } else {
            this.getRol(entidadId);
          }
        });
    }
  }

  verifyEntityByUpdated(entidadId) {
    this.authenticationService.verifyEntityUpdated(entidadId).subscribe(
      (res) => {
        if (res === '0') {
          this.router.navigateByUrl('/pages/home');
        } else {
          this.router.navigateByUrl('/pages');
        }
      },
      (err) => {
        this.authenticationService.clearUser();
        this.toast.showToast(err.message, 'danger');
      }
    );
  }

  verifyEntityByUpdatedRol(entidadId, idRol) {
    this.authenticationService
      .verifyEntityRolUpdated(entidadId, idRol)
      .subscribe(
        (res) => {
          if (res === '0') {
            this.router.navigateByUrl('/pages/home');
          } else {
            this.router.navigateByUrl('/pages');
          }
        },
        (err) => {
          this.authenticationService.clearUser();
          this.toast.showToast(err.message, 'danger');
        }
      );
  }

  openModalEntidad(entidades: any[], rolId: number) {
    this.rolId = rolId;
    const add = this.dialog.open(EntidadComponent, {
      disableClose: true,
      data: entidades
    });
    add.afterClosed().subscribe((any) => {
      if (any) {
        this.entidadId = any.entidadId;
        this.getObtenerRoles(any.entidadId, rolId);
      } else {
        this.logging = false;
        this.authenticationService.clearUser();
        this.toast.showToast('No seleccionó una entidad', 'danger');
      }
    });
  }


  openModalPerfil(roles: any[], entidadId: number) {
    this.entidadId = entidadId;
    const add = this.dialog.open(PerfilComponent, {
      disableClose: true,
      data: roles
    });
    add.afterClosed().subscribe((any) => {
      if (any) {
        this.perfil = any;
        this.getObtenerRoles(entidadId, any.rolId);
      } else {
        this.logging = false;
        this.authenticationService.clearUser();
        this.toast.showToast('No seleccionó un perfil', 'danger');
      }
    });
  }

  getRol(entidadId: number) {
    this.authenticationService.getRolId(entidadId).subscribe((item) => {
      this.verifyEntityByUpdatedRol(entidadId, item[0].rolId);
    });
  }

  registrarEntidad() {
    this.router.navigateByUrl('/regentext/solicitud');
  }

}
