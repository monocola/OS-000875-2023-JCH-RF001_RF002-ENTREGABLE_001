import { Component, OnInit } from '@angular/core';
import { FormControl, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

@Component({
  selector: 'serv-talento-forgot-password',
  templateUrl: './forgot-password.component.html',
  styleUrls: ['./forgot-password.component.scss'],
})
export class ForgotPasswordComponent implements OnInit {
  numeroDocumento = new FormControl('', [
    Validators.required,
    Validators.pattern(/^[a-zA-Z0-9]*$/),
    Validators.minLength(8),
    Validators.maxLength(8),
  ]);

  constructor(
    private toast: ToastService,
    private authenticationService: AuthenticationRepository,
    private router: Router
  ) {}

  ngOnInit(): void {}

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
            this.toast.showToast(err.message, 'danger');
          }
        );
    }
  }
}
