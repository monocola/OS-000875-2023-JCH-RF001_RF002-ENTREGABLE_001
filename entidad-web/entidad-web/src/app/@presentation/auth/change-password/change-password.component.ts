import { Component, OnDestroy } from '@angular/core';
import { AbstractControl, FormBuilder, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

@Component({
  selector: 'serv-talento-change-password',
  templateUrl: './change-password.component.html',
  styleUrls: ['./change-password.component.scss'],
})
export class ChangePasswordComponent implements OnDestroy {
  showPassword = false;
  showNewPassword = false;
  showNewPassword2 = false;
  loading = false;

  form = this.formBuilder.group(
    {
      actualPassword: [
        '',
        [
          Validators.required,
          Validators.minLength(4),
          Validators.maxLength(20),
        ],
      ],
      newPassword: [
        '',
        [
          Validators.required,
          Validators.minLength(4),
          Validators.maxLength(20),
        ],
      ],
      newPasswordRepeated: [
        '',
        [
          Validators.required,
          Validators.minLength(4),
          Validators.maxLength(20),
        ],
      ],
    },
    {
      validators: [this.duplicatePassword],
    }
  );

  duplicatePassword(control: AbstractControl): { passwordDuplicated: boolean } {
    if (
      control.get(['newPassword'])?.value !==
      control.get(['newPasswordRepeated'])?.value
    ) {
      if (control.get(['newPasswordRepeated'])?.value !== '') {
        return { passwordDuplicated: true };
      }
    }
  }

  constructor(
    private formBuilder: FormBuilder,
    private authenticationService: AuthenticationRepository,
    private toast: ToastService,
    private router: Router
  ) {
    if (!sessionStorage.getItem('userDocument')) {
      this.router.navigateByUrl('/auth/login');
    }
  }

  ngOnDestroy(): void {
    sessionStorage.removeItem('userDocument');
  }

  get f() {
    return this.form.controls;
  }

  toggleShowPassword() {
    this.showPassword = !this.showPassword;
  }

  toggleNewPassword() {
    this.showNewPassword = !this.showNewPassword;
  }

  toggleNewPassword2() {
    this.showNewPassword2 = !this.showNewPassword2;
  }

  changePassword() {
    if (this.form.valid) {
      const actualPassword = this.form.get('actualPassword').value;
      const newPassword = this.form.get('newPassword').value;
      this.authenticationService
        .changePassword(actualPassword, newPassword)
        .subscribe(
          (res) => {
            sessionStorage.removeItem('userDocument');
            this.toast.showToast(
              'La contraseña ha sido cambiada con éxito',
              'primary'
            );
            this.router.navigateByUrl('/auth/login');
          },
          (err) => {
            this.toast.showToast(err.message, 'danger');
          }
        );
    } else {
      this.toast.showToast(
        'Complete correctamente los campos marcados con rojo',
        'danger'
      );
    }
  }

}
