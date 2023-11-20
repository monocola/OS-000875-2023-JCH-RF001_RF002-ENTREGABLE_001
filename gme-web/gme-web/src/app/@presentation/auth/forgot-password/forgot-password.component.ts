import { Component, OnInit } from '@angular/core';
import { FormControl, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { distinctUntilChanged } from 'rxjs/operators';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

@Component({
  selector: 'gme-web-forgot-password',
  templateUrl: './forgot-password.component.html',
  styleUrls: ['./forgot-password.component.scss'],
})
export class ForgotPasswordComponent implements OnInit {
  numeroDocumento = new FormControl('', [
    Validators.required,
    Validators.minLength(8),
    Validators.maxLength(12),
  ]);
  constructor(
    private toast: ToastService,
    private authenticationService: AuthenticationRepository,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.numeroDocumento.valueChanges
      .pipe(distinctUntilChanged())
      .subscribe((newValue: string) => {
        if (newValue.length <= 7 || newValue.length > 8) {
          this.numeroDocumento.setValidators([
            Validators.required,
            Validators.minLength(8),
            Validators.maxLength(12),
          ]);
        } else if (newValue.length === 8) {
          this.numeroDocumento.setValidators([
            Validators.required,
            Validators.minLength(8),
            Validators.maxLength(12),
            Validators.pattern(/^-?(|[0-9]\d*)?$/),
          ]);
        }

        this.numeroDocumento.markAsTouched();
        this.numeroDocumento.updateValueAndValidity();
      });
  }

  sendData() {
    if (this.numeroDocumento.valid) {
      this.authenticationService
        .forgotPassword(this.numeroDocumento.value)
        .subscribe(
          (res) => {
            this.toast.showToast('Por favor revisar el correo', 'success');
            this.router.navigateByUrl('/auth/login');
          },
          (err) => {
            console.log(err);
            this.toast.showToast(
              err.messages[0] === null
                ? 'Ha ocurrido un error inesperado en el servidor'
                : err.message,
              'danger'
            );
          }
        );
    }
  }
}
