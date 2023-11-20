import { AfterViewInit, Component, OnInit } from '@angular/core';
import { Validators, FormBuilder, FormGroup } from '@angular/forms';
import { Router } from '@angular/router';
import { Const } from 'src/app/@data/services/const';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { getHash } from 'src/app/utils/general';
declare var $: any;

@Component({
  selector: 'serv-talento-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.scss'],
})
export class LoginComponent implements OnInit, AfterViewInit {
  showPassword = false;
  logging = false;

  loginForm: FormGroup = this.formBuilder.group({
    documentNumber: [
      '',
      [Validators.required, Validators.minLength(8), Validators.maxLength(12)],
    ],
    password: [
      '',
      [Validators.required, Validators.minLength(4), Validators.maxLength(30)],
    ],
    inputCaptcha: ['', [Validators.required, Validators.minLength(5)]],
  });

  get f() {
    return this.loginForm.controls;
  }

  constructor(
    private formBuilder: FormBuilder,
    private authenticationService: AuthenticationRepository,
    private toast: ToastService,
    private router: Router
  ) {}

  ngAfterViewInit(): void {
    setTimeout(() => {
      this.inicializarCaptcha();
    }, 1000);
  }

  ngOnInit(): void {}

  toggleShowPassword() {
    this.showPassword = !this.showPassword;
  }

  login() {
    if (this.loginForm.valid && !this.logging) {
      this.logging = true;
      const valorCaptcha = $('#captchaLogin').realperson('getHash');
      if (valorCaptcha === getHash(this.loginForm.get('inputCaptcha').value)) {
        const userForm = this.loginForm.get('documentNumber').value;
        const passwordForm = this.loginForm.get('password').value;
        this.authenticationService
          .login(userForm, passwordForm)
          .toPromise()
          .then((res) => {
            this.logging = false;
            const user = this.authenticationService.getCurrentUserValue;
            if (user.rolId === Const.R_ADMIN_SERVIR) {
              this.router.navigateByUrl('/pages');
            } else {
              this.verifyEntityUpdated();
            }
          })
          .catch((err) => {
            this.logging = false;
            this.authenticationService.clearUser();
            if (err && err.message) {
              this.toast.showToast(err.message, 'danger');
            }
          });
      } else {
        this.logging = false;
        this.loginForm.get('inputCaptcha').setValue('');
        this.toast.showToast('El valor del captcha es incorrecto', 'danger');
      }
    }
  }

  verifyEntityUpdated() {
    this.authenticationService
      .verifyEntityUpdated(
        this.authenticationService.getCurrentUserValue.entidadId
      )
      .subscribe(
        (res) => {
          if (res === '0') {
            this.router.navigateByUrl('/pages/entidad');
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
}
