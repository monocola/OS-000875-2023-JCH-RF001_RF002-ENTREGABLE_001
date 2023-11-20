import { STEPPER_GLOBAL_OPTIONS } from '@angular/cdk/stepper';
import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { FormGroup } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { PerfilesRepository } from 'src/app/@domain/repository/perfiles.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { ModalPreventCloseComponent } from '../components/modal-prevent-close/modal-prevent-close.component';
import { HelperLey1401Service } from './helperLey1401.service';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { Const } from 'src/app/@data/services/const';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';

@Component({
  selector: 'serv-talento-ley1401',
  templateUrl: './ley1401.component.html',
  styleUrls: ['./ley1401.component.scss'],
  providers: [
    {
      provide: STEPPER_GLOBAL_OPTIONS,
      useValue: { displayDefaultIndicatorType: false },
    },
  ],
})
export class Ley1401Component implements OnInit {
  @ViewChild('myDiv') theDiv: ElementRef;

  msgTooltip =
    'Podrás registrar de forma masiva los perfiles de puesto y prácticas';
  bodySize = '1000px';
  mode = 0;

  constructor(
    private dialog: MatDialog,
    private toastService: ToastService,
    public helper1401Service: HelperLey1401Service,
    public perfilesService: PerfilesRepository,
    private perfilesRepository: PerfilesRepository,
    public router: Router,
    public authenticationRepository: AuthenticationRepository

  ) {}

  ngOnInit(): void {
    this.helper1401Service.loadCombox();
    this.validateAndGetRegimenSelected();
  }

  validateAndGetRegimenSelected() {
    if (!this.helper1401Service.regimenSelected) {
      const val = sessionStorage.getItem('regimenSelected');
      val
        ? (this.helper1401Service.regimenSelected = JSON.parse(val))
        : this.router.navigateByUrl('pages/regperfilpuesto/creacion');
    }
  }

  changeMode(mode: number) {
    this.mode = mode;
  }

  nextStep() {
    window.scrollTo(0, 0);
    let form = null;
    switch (this.helper1401Service.indexStepper) {
      case 0:
        form = this.helper1401Service.identificacionForm;
        form.markAllAsTouched();
        this.saveOrUpdateIdentificacionForm(form);
        break;
      case 1:
        form = this.helper1401Service.funcionesForm;
        form.markAllAsTouched();
        this.saveOrUpdateFunctionForm(form);
        break;
      case 2:
        form = this.helper1401Service.formacionAcademicaForm;
        form.markAllAsTouched();
        this.saveOrUpdateFormacion(form);
        break;
      default:
        break;
    }
  }

  previousStep() {
    window.scrollTo(0, 0);
    this.helper1401Service.indexStepper--;
  }

  createProfile() {
    let form = this.helper1401Service.experienciaForm;
    form.markAllAsTouched();
    this.saveOrUpdateExperiencia(form);
  }

  onResized(e) {
    this.bodySize = e.newWidth + 'px';
  }

  change(e) {
    window.scrollTo(0, 0);
    this.helper1401Service.indexStepper = e.selectedIndex;
  }

  saveOrUpdateIdentificacionForm(form: FormGroup) {
    if (form.invalid) return;
    if (form.pristine && this.helper1401Service.idIdentificacion) {
      this.helper1401Service.indexStepper = 1;
    } else {
      const body = form.getRawValue();
      body.regimenId = this.helper1401Service.regimenSelected.maeDetalleId;
      body.codigoRegimen = this.helper1401Service.regimenSelected.codProg;
      this.perfilesRepository
        .saveOrUpdateIdentificacion(
          body,
          body.regimenId,
          this.helper1401Service.idIdentificacion
        )
        .subscribe(
          (res) => {
            !this.helper1401Service.idIdentificacion
              ? this.toastService.showToast(
                  'Perfil creado correctamente',
                  'primary'
                )
              : this.toastService.showToast(
                  'Cambios guardados correctamente',
                  'primary'
                );
            this.helper1401Service.setIdentificacionData(res);
            setTimeout(() => {
              this.helper1401Service.indexStepper = 1;
            }, 0);
          },
          (err) => this.toastService.showToast(err, 'danger')
        );
    }
  }

  saveOrUpdateFunctionForm(form: FormGroup) {
    if (form.invalid) return;
    if (form.pristine === true && this.helper1401Service.idFunciones) {
      this.helper1401Service.indexStepper = 2;
    } else {
      const body = form.getRawValue();
      body.funcionesToDelete = this.helper1401Service.funcionesToDelete || [];
      this.perfilesRepository
        .saveOrUpdateFunciones(
          body,
          this.helper1401Service.regimenSelected.codProg,
          this.helper1401Service.idIdentificacion || null,
          this.helper1401Service.idFunciones || null
        )
        .subscribe(
          (res) => {
            this.helper1401Service.setFuncionesData(res);
            this.toastService.showToast(
              'Cambios guardados correctamente',
              'primary'
            );
            setTimeout(() => {
              this.helper1401Service.indexStepper = 2;
            }, 0);
          },
          (err) => this.toastService.showToast(err, 'danger')
        );
    }
  }

  saveOrUpdateFormacion(form) {
    if (form.invalid) return;
    if (form.pristine === true && this.helper1401Service.idFormacion) {
      this.helper1401Service.indexStepper = 3;
    } else {
      const body = form.getRawValue();
      let conocimientosBasicos: any[] = [];

      body.regimen = this.helper1401Service.regimenSelected.maeDetalleId;
      body.perfil = this.helper1401Service.idIdentificacion;
      body.nivelesAcademicos = body.nivelesAcademicos.concat(
        body.nivelesAcademicosToDelete
      );

      body.conocimientosBasicos = body.conocimientosBasicos.concat (this.helper1401Service.formacionDeletedItems);
      body.conocimientosBasicos.forEach((item: any) => {
        let response = this.helper1401Service.listaoriginal.find(
          (x) => x.descripcion === item.descripcion
        );

        if (response) {
          item.maeConocimientoId = response.maeConocimientoId;
          conocimientosBasicos.push(item);
        }
      });

      body.conocimientosBasicos = conocimientosBasicos;
      console.log("body: ",body);
      console.log("body.regimen: ",body.regimen);
      console.log("body.perfil: ",body.perfil);

      this.perfilesRepository
        .saveOrUpdateFormacion(body, body.regimen, body.perfil)
        .toPromise()
        .then(
          () => {
            this.perfilesRepository
              .getFormacionData(this.helper1401Service.idIdentificacion)
              .subscribe((formacionData) => {
                console.log("formacionData: ",formacionData);
                this.helper1401Service.formacionDeletedItems = [];
                this.helper1401Service.setFormacionData(formacionData);
                this.toastService.showToast(
                  'Cambios guardados correctamente',
                  'primary'
                );
                setTimeout(() => {
                  this.helper1401Service.indexStepper = 3;
                }, 0);
              });
          },
          (err) => this.toastService.showToast(err, 'danger')
        );
    }
  }

  saveOrUpdateExperiencia(form) {
    if (form.invalid) return;

      const body = form.getRawValue();
      body.habilidadesToDelete = this.helper1401Service.habilidadesToDelete;
      this.perfilesRepository
        .saveOrUpdateExperiencia(
          body,
          this.helper1401Service.idIdentificacion,
          this.helper1401Service.idExperiencia
        )
        .subscribe(
          (res) => {
             
            !this.helper1401Service.idExperiencia
              ? this.toastService.showToast(
                  'Perfil creado correctamente',
                  'primary'
                )
              : this.toastService.showToast(
                  'Perfil editado correctamente',
                  'primary'
                );
                if (this.authenticationRepository.isSuperAdminEntidad()) {
                this.verificarPerfil(res.perfilId, this.helper1401Service.identificacionForm.value.nombrePerfil);
                } else {
                  setTimeout(() => {
                    this.helper1401Service.registroTerminado = true;
                    this.router.navigateByUrl('pages/regperfilpuesto');
                    this.helper1401Service.initializeValues();
                  }, 0);
                }
          },
          (err) => this.toastService.showToast(err, 'danger')
        );
    
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
              this.helper1401Service.registroTerminado = true;
              this.router.navigateByUrl('pages/regperfilpuesto');
              this.helper1401Service.initializeValues();
            }, 0);
          })
          .catch((error: any) => {});
      } else {
        setTimeout(() => {
          this.helper1401Service.registroTerminado = true;
          this.router.navigateByUrl('pages/regperfilpuesto');
          this.helper1401Service.initializeValues();
        }, 0);
      }
    });

}

  async canDeactivate() {
    if (this.helper1401Service.regimenSelected) {
      const confirmDialog = this.dialog.open(ModalPreventCloseComponent, {
        data: { stepActual: this.helper1401Service.indexStepper },
      });
      const result = await confirmDialog.afterClosed().toPromise();
      if (result === true) {
        this.helper1401Service.initializeValues();
        return true;
      } else return false;
    } else return true;
  }
}
