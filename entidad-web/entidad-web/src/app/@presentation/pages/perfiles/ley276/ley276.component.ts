import { STEPPER_GLOBAL_OPTIONS } from '@angular/cdk/stepper';
import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { AbstractControl, FormGroup } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { PerfilesRepository } from 'src/app/@domain/repository/perfiles.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { ModalPreventCloseComponent } from '../components/modal-prevent-close/modal-prevent-close.component';
import { HelperLey276Service } from './helperLey276.service';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { Const } from 'src/app/@data/services/const';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';

@Component({
  selector: 'serv-talento-ley276',
  templateUrl: './ley276.component.html',
  styleUrls: ['./ley276.component.scss'],
  providers: [
    {
      provide: STEPPER_GLOBAL_OPTIONS,
      useValue: { displayDefaultIndicatorType: false },
    },
  ],
})
export class Ley276Component implements OnInit {
  @ViewChild('myDiv') theDiv: ElementRef;

  msgTooltip =
    'Podrás registrar de forma masiva los perfiles de puesto y prácticas';
  bodySize = '1000px';
  mode = 0;

  constructor(
    private dialog: MatDialog,
    private toastService: ToastService,
    public helperService: HelperLey276Service,
    private perfilesRepository: PerfilesRepository,
    public router: Router,
    public authenticationRepository: AuthenticationRepository

  ) {}

  ngOnInit(): void {
    this.validateAndGetRegimenSelected();
    this.helperService.initializeForm();
  }

  validateAndGetRegimenSelected() {
    if (!this.helperService.regimenSelected) {
      const val = sessionStorage.getItem('regimenSelected');
      val
        ? (this.helperService.regimenSelected = JSON.parse(val))
        : this.router.navigateByUrl('pages/regperfilpuesto/creacion');
    }
  }

  changeMode(mode: number) {
    this.mode = mode;
  }

  nextStep() {
    window.scrollTo(0, 0);
    let form = null;
    switch (this.helperService.indexStepper) {
      case 0:
        form = this.helperService.identificacionForm;
        form.markAllAsTouched();
        this.saveOrUpdateIdentificacionForm(form);
        break;
      case 1:
        form = this.helperService.funcionesForm;
        form.markAllAsTouched();
        this.saveOrUpdateFunctionForm(form);
        break;
      case 2:
        form = this.helperService.formacionAcademicaForm;
        form.markAllAsTouched();
        this.saveOrUpdateFormacion(form);
        break;
      default:
        break;
    }
  }

  previousStep() {
    window.scrollTo(0, 0);
    this.helperService.indexStepper--;
  }

  createProfile() {
    let form = this.helperService.experienciaForm;
    form.markAllAsTouched();
    this.saveOrUpdateExperiencia(form);
  }

  onResized(e) {
    this.bodySize = e.newWidth + 'px';
  }

  change(e) {
    window.scrollTo(0, 0);
    this.helperService.indexStepper = e.selectedIndex;
  }

  saveOrUpdateIdentificacionForm(form: FormGroup) {
    if (form.invalid) return;
    if (form.pristine && this.helperService.idIdentificacion) {
      this.helperService.indexStepper = 1;
    } else {
      const body = form.getRawValue();
      body.regimenId = this.helperService.regimenSelected.maeDetalleId;
      body.codigoRegimen = this.helperService.regimenSelected.codProg;
      
      this.perfilesRepository
        .saveOrUpdateIdentificacion(
          body,
          body.regimenId,
          this.helperService.idIdentificacion
        )
        .subscribe(
          (res) => {
            !this.helperService.idIdentificacion
              ? this.toastService.showToast(
                  'Perfil creado correctamente',
                  'primary'
                )
              : this.toastService.showToast(
                  'Cambios guardados correctamente',
                  'primary'
                );
            this.helperService.setIdentificacionData(res);
            setTimeout(() => {
              this.helperService.indexStepper = 1;
            }, 0);
          },
          (err) => this.toastService.showToast(err, 'danger')
        );
    }
  }

  saveOrUpdateFunctionForm(form: FormGroup) {
    if (form.invalid) return;
    if (form.pristine && this.helperService.idIdentificacion) {
      this.helperService.indexStepper = 2;
    } else {
      const body = form.getRawValue();
      body.funcionesToDelete = this.helperService.funcionesToDelete || [];
      this.perfilesRepository
        .saveOrUpdateFunciones(
          body,
          this.helperService.regimenSelected?.codProg,
          this.helperService.idIdentificacion || null,
          this.helperService.idFunciones || null
        )
        .subscribe(
          (res) => {
            this.helperService.setFuncionesData(res);
            this.toastService.showToast(
              'Cambios guardados correctamente',
              'primary'
            );
            setTimeout(() => {
              this.helperService.indexStepper = 2;
            }, 0);
          },
          (err) => this.toastService.showToast(err, 'danger')
        );
    }
  }

  saveOrUpdateFormacion(form) {
    if (form.invalid) return;
    const body = form.getRawValue();
    body.regimen = this.helperService.regimenSelected.maeDetalleId;
    body.perfil = this.helperService.idIdentificacion;
    this.perfilesRepository
      .saveOrUpdateFormacion(body, body.regimen, body.perfil)
      .subscribe(
        () => {
          this.perfilesRepository
            .getFormacionData(this.helperService.idIdentificacion)
            .subscribe(async (formacionData) => {
              await this.helperService.getCoursesAndProgramas();
              this.helperService.setFormacionData(formacionData);
              this.toastService.showToast(
                'Cambios guardados correctamente',
                'primary'
              );
              this.helperService.indexStepper = 3;
            });
        },
        (err) => this.toastService.showToast(err, 'danger')
      );
  }

  detectNewCourse(arr1: any[], arr2: any[]) {
    let nuevo = false;
    const array = arr1.concat(arr2);
    array.forEach((el) => {
      if (typeof el.nombreCurso.value === 'string') {
        nuevo = true;
      }
    });
    return nuevo;
  }

  saveOrUpdateExperiencia(form) {
    if (form.invalid) return;
    if (form.pristine && this.helperService.idExperiencia) {
      this.toastService.showToast(
        'No se ha realizado cambio alguno',
        'primary'
      );
      this.helperService.registroTerminado = true;
      this.router.navigateByUrl('pages/regperfilpuesto');
      this.helperService.initializeValues();
    } else {
      const body = form.getRawValue();
      body.habilidadesToDelete = this.helperService.habilidadesToDelete;
      body.requisitosToDelete = this.helperService.requisitosToDelete;
      this.perfilesRepository
        .saveOrUpdateExperiencia(
          body,
          this.helperService.idIdentificacion || 5,
          this.helperService.idExperiencia
        )
        .subscribe(
          (res) => {
            !this.helperService.idExperiencia
              ? this.toastService.showToast(
                  'Perfil creado correctamente',
                  'primary'
                )
              : this.toastService.showToast(
                  'Perfil editado correctamente',
                  'primary'
                );
            if (this.authenticationRepository.isSuperAdminEntidad()) {
              this.verificarPerfil(res.perfilId, this.helperService.identificacionForm.value.nombrePerfil);
            } else {
              setTimeout(() => {
                this.helperService.registroTerminado = true;
                this.router.navigateByUrl('pages/regperfilpuesto');
                this.helperService.initializeValues();
              }, 0);
            }
                        
          },
          (err) => this.toastService.showToast(err, 'danger')
        );
    }
  }

  verificarPerfil(perfilId: any, nombrePerfil:any) {

    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        rutaImagen: 'assets/images/icons/check.png',
        title: 'Verificación de perfil',
        bodyHtmlText: `Confirma que los <strong>datos</strong> ingresados en el perfil "<span class="color-primary">${nombrePerfil}</span>" son <strong>correctos</strong>`,
      },
    });

    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.perfilesRepository
          .revisarPerfil(perfilId, Const.EST_PERFILES_REVISADO)
          .toPromise()
          .then((res: any) => {
            setTimeout(() => {
              this.helperService.registroTerminado = true;
              this.router.navigateByUrl('pages/regperfilpuesto');
              this.helperService.initializeValues();
            }, 0);
          })
          .catch((error: any) => {});
      } else {
        setTimeout(() => {
          this.helperService.registroTerminado = true;
          this.router.navigateByUrl('pages/regperfilpuesto');
          this.helperService.initializeValues();
        }, 0);
      }
    });

}

  getNombreCurso(control: AbstractControl) {
    const cursoControl = control.value;
    if (typeof cursoControl === 'object') {
      return cursoControl?.descripcion;
    }
    if (typeof cursoControl === 'string') {
      return cursoControl;
    }
  }

  async canDeactivate() {
    if (this.helperService.regimenSelected) {
      const confirmDialog = this.dialog.open(ModalPreventCloseComponent, {
        data: { stepActual: this.helperService.indexStepper },
      });
      const result = await confirmDialog.afterClosed().toPromise();
      if (result === true) {
        this.helperService.initializeValues();
        return true;
      } else return false;
    } else return true;
  }
}
