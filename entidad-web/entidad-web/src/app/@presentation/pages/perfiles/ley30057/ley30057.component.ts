import { STEPPER_GLOBAL_OPTIONS } from '@angular/cdk/stepper';
import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { AbstractControl, FormControl, FormGroup } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { PerfilesRepository } from 'src/app/@domain/repository/perfiles.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { ModalPreventCloseComponent } from '../components/modal-prevent-close/modal-prevent-close.component';
import { HelperLey30057Service } from './helperLey30057.service';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { Const } from 'src/app/@data/services/const';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';

@Component({
  selector: 'serv-talento-ley30057',
  templateUrl: './ley30057.component.html',
  styleUrls: ['./ley30057.component.scss'],
  providers: [
    {
      provide: STEPPER_GLOBAL_OPTIONS,
      useValue: { displayDefaultIndicatorType: false },
    },
  ],
})
export class Ley30057Component implements OnInit {
  @ViewChild('myDiv') theDiv: ElementRef;

  bodySize = '62.5rem';
  mode = 0;
  msgTooltip =
    'Podrás registrar de forma masiva los perfiles de puesto y prácticas';

  constructor(
    private dialog: MatDialog,
    private perfilesRepository: PerfilesRepository,
    private toastService: ToastService,
    public router: Router,
    public perfilesService: PerfilesRepository,
    public helper30057Service: HelperLey30057Service,
    public authenticationRepository: AuthenticationRepository
  ) {}

  ngOnInit(): void {
    this.helper30057Service.initializeForms();
    this.validateAndGetRegimenSelected();
  }

  validateAndGetRegimenSelected() {
    if (!this.helper30057Service.regimenSelected) {
      const val = sessionStorage.getItem('regimenSelected');
      val
        ? (this.helper30057Service.regimenSelected = JSON.parse(val))
        : this.router.navigateByUrl('pages/regperfilpuesto/creacion');
    }
  }

  changeMode(mode: number) {
    this.mode = mode;
  }

  nextStep() {
    window.scrollTo(0, 0);
    let form = null;
    switch (this.helper30057Service.indexStepper) {
      case 0:
        form = this.helper30057Service.identificacionForm;
        form.markAllAsTouched();
        this.saveOrUpdateIdentificacionForm(form);
        break;
      case 1:
        form = this.helper30057Service.funcionesForm;
        form.markAllAsTouched();
        this.saveOrUpdateFunctionForm(form);
        break;
      case 2:
        form = this.helper30057Service.formacionAcademicaForm;
        form.markAllAsTouched();
        this.saveOrUpdateFormacion(form);
        break;
      default:
        break;
    }
  }

  saveOrUpdateIdentificacionForm(form: FormGroup) {
    if (form.invalid) return;
    if (form.pristine && this.helper30057Service.idIdentificacion) {
      this.helper30057Service.indexStepper = 1;
    } else {
      const body = form.getRawValue();
      body.regimenId = this.helper30057Service.regimenSelected.maeDetalleId;
      body.codigoRegimen = this.helper30057Service.regimenSelected.codProg;
      this.perfilesRepository
        .saveOrUpdateIdentificacion(
          body,
          body.regimenId,
          this.helper30057Service.idIdentificacion
        )
        .subscribe(
          (res) => {
            this.helper30057Service.setIdentificacionData(res);
            !this.helper30057Service.idIdentificacion
              ? this.toastService.showToast(
                  'Perfil creado correctamente',
                  'primary'
                )
              : this.toastService.showToast(
                  'Cambios guardados correctamente',
                  'primary'
                );
            setTimeout(() => {
              this.helper30057Service.indexStepper = 1;
            }, 0);
          },
          (err) => this.toastService.showToast(err, 'danger')
        );
    }
  }

  saveOrUpdateFunctionForm(form: FormGroup) {
    if (form.invalid) return;
    if (form.pristine && this.helper30057Service.idFunciones) {
      this.helper30057Service.indexStepper = 2;
    } else {
      const body = form.getRawValue();
      body.funcionesToDelete = this.helper30057Service.funcionesToDelete || [];
      this.perfilesRepository
        .saveOrUpdateFunciones(
          body,
          this.helper30057Service.regimenSelected?.codProg,
          this.helper30057Service.idIdentificacion || null,
          this.helper30057Service.idFunciones || null
        )
        .subscribe(
          (res) => {
            this.toastService.showToast(
              'Cambios guardados correctamente',
              'primary'
            );
            this.helper30057Service.setFuncionesData(res);
            setTimeout(() => {
              this.helper30057Service.indexStepper = 2;
            }, 0);
          },
          (err) => this.toastService.showToast(err, 'danger')
        );
    }
  }

  saveOrUpdateFormacion(form: FormGroup) {
    if (form.invalid) return;
    const body = form.getRawValue();
    body.regimen = this.helper30057Service.regimenSelected.maeDetalleId;
    body.perfil = this.helper30057Service.idIdentificacion;   
    this.perfilesRepository
      .saveOrUpdateFormacion(body, body.regimen, body.perfil)
      .subscribe(
        () => {
          this.perfilesRepository
            .getFormacionData(this.helper30057Service.idIdentificacion)
            .subscribe(async (formacionData) => {
              this.helper30057Service.setFormacionData(formacionData);
              this.toastService.showToast(
                'Cambios guardados correctamente',
                'primary'
              );
              this.helper30057Service.indexStepper = 3;
            });
        },
        (err) => this.toastService.showToast(err, 'danger')
      );
  }

  detectNewCourse(arr1: any[], arr2: any[]) {
    let nuevo = false;
    arr1.concat(arr2).forEach((el) => {
      if (typeof el.nombreCurso.value === 'string') nuevo = true;
    });
    return nuevo;
  }

  saveOrUpdateExperiencia(form) {
    if (form.invalid) return;
    if (form.pristine && this.helper30057Service.idExperiencia) {
      this.toastService.showToast(
        'No se ha realizado cambio alguno',
        'primary'
      );
      this.helper30057Service.registroTerminado = true;
      this.router.navigateByUrl('pages/regperfilpuesto');
      this.helper30057Service.initializeValues();
    } else {
      const body = form.getRawValue();
      body.habilidadesToDelete = this.helper30057Service.habilidadesToDelete;
      body.requisitosToDelete = this.helper30057Service.requisitosToDelete;
      this.perfilesRepository
        .saveOrUpdateExperiencia(
          body,
          this.helper30057Service.idIdentificacion,
          this.helper30057Service.idExperiencia
        )
        .subscribe(
          (res) => {
            !this.helper30057Service.idExperiencia
              ? this.toastService.showToast(
                  'Perfil creado correctamente',
                  'primary'
                )
              : this.toastService.showToast(
                  'Perfil editado correctamente',
                  'primary'
                );
            if (this.authenticationRepository.isSuperAdminEntidad()) {
              this.verificarPerfil(res.perfilId, this.helper30057Service.identificacionForm.value.nombrePerfil);
            } else {
              setTimeout(() => {
                this.helper30057Service.registroTerminado = true;
                this.router.navigateByUrl('pages/regperfilpuesto');
                this.helper30057Service.initializeValues();
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
        this.perfilesService
          .revisarPerfil(perfilId, Const.EST_PERFILES_REVISADO)
          .toPromise()
          .then((res: any) => {
            setTimeout(() => {
              this.helper30057Service.registroTerminado = true;
              this.router.navigateByUrl('pages/regperfilpuesto');
              this.helper30057Service.initializeValues();
            }, 0);
          })
          .catch((error: any) => {});
      } else {
        setTimeout(() => {
          this.helper30057Service.registroTerminado = true;
          this.router.navigateByUrl('pages/regperfilpuesto');
          this.helper30057Service.initializeValues();
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

  previousStep() {
    window.scrollTo(0, 0);
    this.helper30057Service.indexStepper--;
  }

  createProfile() {
    let form = this.helper30057Service.experienciaForm;
    form.markAllAsTouched();
    this.saveOrUpdateExperiencia(form);
  }

  onResized(e) {
    this.bodySize = e.newWidth + 'px';
  }

  change(e) {
    window.scrollTo(0, 0);
    this.helper30057Service.indexStepper = e.selectedIndex;
  }

  async canDeactivate() {
    if (this.helper30057Service.regimenSelected) {
      if (!this.helper30057Service.registroTerminado) {
        const confirmDialog = this.dialog.open(ModalPreventCloseComponent, {
          data: { stepActual: this.helper30057Service.indexStepper },
        });
        const result = await confirmDialog.afterClosed().toPromise();
        if (result === true) {
          this.helper30057Service.initializeValues();
          return true;
        } else return false;
      } else return true;
    } else return true;
  }
}
