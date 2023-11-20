import { Component, OnDestroy } from '@angular/core';
import { AbstractControl, FormBuilder, Validators } from '@angular/forms';
import { MatDialogRef } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { ModalConfirmationComponent } from '../../../@common-components/modal-confirmation/modal-confirmation.component';
import { MatDialog } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-cambio-contra',
  templateUrl: './modal-cambio-contra.component.html',
})
export class ModalCambioContraComponent implements OnDestroy {
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
    private dialogRef: MatDialogRef<ModalCambioContraComponent>,
    private authenticationService: AuthenticationRepository,
    private toast: ToastService,
    private router: Router,
    private dialog: MatDialog,
  ) {
    if (!sessionStorage.getItem('userDocument')) {
      this.router.navigateByUrl('');
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

  onNoClick(data = false) {
    this.dialogRef.close(data);
  }

  changePassword() {
    if (this.form.valid) {
      this.getUserEmail ().then ((correo: any) => {
        const actualPassword = this.form.get('actualPassword').value;
        const newPassword = this.form.get('newPassword').value;
        const currentUser = this.authenticationService.getCurrentUserValue;
        const temp = currentUser.token;
        currentUser.token = null;
        this.authenticationService
          .changePasswordPerfil(actualPassword, newPassword)
          .subscribe(
            (res) => {
              currentUser.token = temp;

              if (correo !== null) {
                this.authenticationService
                .getUserCorreo(correo)
                .subscribe(
                  (resp) => {
                    this.openAlertModal ();
                  }
                );
              } else {
                this.openAlertModal ();
              }
            },
            (err) => {
              this.toast.showToast(err.message, 'danger');
              currentUser.token = temp;
            }
          );
      });
    } else {
      this.toast.showToast(
        'Complete correctamente los campos marcados con rojo',
        'danger'
      );
    }
  }

  async openAlertModal () {
    this.dialogRef.close(null);

    const modal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'La contraseña ha sido cambiada con éxito',
        bodyText: 'Se procedera a cerrar la sesión',
        rutaImagen: 'assets/images/icons/done.svg',
        visibleCancel: false
      },
    });

    modal.afterClosed().subscribe((res) => {
      this.router.navigateByUrl('/auth/login');
    });
  }

  async getUserEmail (): Promise<any> {
    return await new Promise((resolve, reject) => {
      this.authenticationService.getUserData ().toPromise ().then ((res: any) => {
        resolve (res.payload.correoElectronico);
      }).catch ((error: any) => {
        resolve (null);
      });
    });
  }
}
