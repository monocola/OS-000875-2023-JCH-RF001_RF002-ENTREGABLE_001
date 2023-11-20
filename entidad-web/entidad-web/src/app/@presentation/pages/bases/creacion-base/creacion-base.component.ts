import { STEPPER_GLOBAL_OPTIONS } from '@angular/cdk/stepper';
import { Component, OnDestroy, OnInit } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { Const } from 'src/app/@data/services/const';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { BasesRepository } from 'src/app/@domain/repository/bases.repository';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { ModalPreventCloseComponent } from '../../perfiles/components/modal-prevent-close/modal-prevent-close.component';
import { BaseRevisadaModalComponent } from '../components/base-revisada-modal/base-revisada-modal.component';
import { ModalAddObservacionComponent } from '../components/modal-add-observacion/modal-add-observacion.component';
import { ModalBaseObsLevantadasComponent } from '../components/modal-base-obs-levantadas/modal-base-obs-levantadas.component';
import {
  CreacionBaseService,
  ObservacionStepBase,
} from './creacion-base.service';
import { FormGroup } from '@angular/forms';

@Component({
  selector: 'serv-talento-creacion-base',
  templateUrl: './creacion-base.component.html',
  styleUrls: ['./creacion-base.component.scss'],
  providers: [
    {
      provide: STEPPER_GLOBAL_OPTIONS,
      useValue: { displayDefaultIndicatorType: false },
    },
    {
      provide: STEPPER_GLOBAL_OPTIONS,
      useValue: { showError: true },
    },
  ],
})
export class CreacionBaseComponent implements OnInit, OnDestroy {
  bodySize = '1000px';
  mode = 0;
  observaciones: ObservacionStepBase[] = [];
  editSuscription = new Subscription();
  const = Const;

  constructor(
    public helperService: CreacionBaseService,
    public router: Router,
    private toastService: ToastService,
    private dialog: MatDialog,
    private basesService: BasesRepository,
    public authenticationRepository: AuthenticationRepository
  ) {}

  ngOnDestroy(): void {
    this.helperService.editEmitter.next(null);
    this.editSuscription.unsubscribe();
  }

  ngOnInit(): void {
    this.validateAndGetJerarquia();
    this.helperService.initializeForm();
    this.helperService.loadCombox();
    if (this.helperService.disableAllFields) {
      this.helperService.form1.disable();
      this.helperService.form2.disable();
      this.helperService.form3.disable();
      this.helperService.form4.disable();
      this.helperService.form5.disable();
      this.helperService.form6.disable();
    }
    if (
      (this.authenticationRepository.isCoordinador() || this.authenticationRepository.isSuperAdminEntidad()) &&
      (this.helperService.estadoBase === Const.ETA_BASE_POR_PUBLICAR ||
        this.helperService.estadoBase === Const.ETA_BASE_REVISADO)
    ) {
      this.helperService.observacionesInicializar();
    }
    if (
      (this.authenticationRepository.isGestor() || this.authenticationRepository.isSuperAdminEntidad()) &&
      this.helperService.estadoBase === Const.ETA_BASE_OBSERVADO
    ) {
      let etapas = [0, 1, 2, 3, 4, 5];
      this.helperService.observaciones.forEach((item) => {
        let index = etapas.findIndex(
          (value) => value === item.step && item.description.length !== 0
        );
        if (index !== -1) {
          etapas = etapas.filter((value, index1) => index !== index1);
        }
      });
      etapas.forEach((item) => {
        switch (item) {
          case 0:
            this.helperService.form1.disable();
            break;
          case 1:
            this.helperService.form2.disable();
            break;
          case 2:
            this.helperService.form3.disable();
            break;
          case 3:
            this.helperService.form4.disable();
            break;
          case 4:
            this.helperService.form5.disable();
            break;
          case 5:
            this.helperService.form6.disable();
            break;
        }
      });
    }
    this.editSuscription = this.helperService.editEmitter.subscribe((res) => {
      if (res) this.observarStep();
    });
  }

  saveObservacionesCorregidas() {
    const idEtapaActual = this.helperService.etapasRegistro.find(
      (eta) => eta.codProg === this.helperService.estadoBase
    ).maeDetalleId;
    const idEtapaNueva = this.helperService.etapasRegistro.find((eta) => {
      return eta.codProg === this.const.ETA_BASE_POR_REVISAR;
    }).maeDetalleId;
    this.basesService
      .saveMovimientosObservaciones(
        idEtapaActual,
        idEtapaNueva,
        this.helperService.idBase,
        []
      )
      .subscribe((res) => {
        const dialog = this.dialog.open(ModalBaseObsLevantadasComponent);
        dialog.afterClosed().subscribe((rest) => {
          this.helperService.initializeValues();
          this.router.navigateByUrl('pages/gestionbases');
        });
      });
  }

  returnObs() {
    return (
      this.helperService.observaciones.filter((obs) => !obs.resuelto).length !==
      0
    );
  }

  validateAndGetJerarquia() {
    if (!this.helperService.jerarquiaSelected) {
      const jerarquia = sessionStorage.getItem('jerarquiaSelected');
      if (jerarquia) {
        this.helperService.jerarquiaSelected = JSON.parse(jerarquia);
        this.helperService.jerarquiaSelected.regimen.codProg === Const.MD_DL1041
          ? (this.helperService.jerarquiaMode = 1)
          : (this.helperService.jerarquiaMode = 0);
      } else {
        this.router.navigateByUrl('pages/gestionbases/elige');
      }
    }
  }

  observarStep() {
    const newObs = this.dialog.open(ModalAddObservacionComponent, {
      width: '500px',
      data: this.helperService.observaciones[this.helperService.indexStepper]
        .description,
    });
    newObs.afterClosed().subscribe((res) => {
      if (res) {
        this.helperService.setObservacion(res);
      }
    });
  }

  saveObservaciones() {
    let obs = [...this.helperService.observaciones];
    obs = obs.filter((o) => o.description && o.resuelto === false);
    const observaciones = obs.map((o) => {
      return {
        etapa: o.step + 1,
        observacion: o.description,
      };
    });
    const idEtapaActual = this.helperService.etapasRegistro.find(
      (eta) => eta.codProg === this.helperService.estadoBase
    ).maeDetalleId;
    const idEtapaNueva = this.helperService.etapasRegistro.find((eta) => {
      if (observaciones.length > 0) {
        return eta.codProg === this.const.ETA_BASE_OBSERVADO;
      } else {
        return eta.codProg === this.const.ETA_BASE_REVISADO;
      }
    }).maeDetalleId;
    this.basesService
      .saveMovimientosObservaciones(
        idEtapaActual,
        idEtapaNueva,
        this.helperService.idBase,
        observaciones
      )
      .subscribe((res) => {
        if (observaciones.length > 0) {
          this.toastService.showToast(
            'Se observó la base correctamente',
            'success'
          );
          this.helperService.initializeValues();
          this.router.navigateByUrl('pages/gestionbases');
        } else {
          const modal = this.dialog.open(BaseRevisadaModalComponent);
          modal.afterClosed().subscribe((rest) => {
            this.publicarBase(this.helperService.baseSeleccionada);
          });
        }
      });
  }

  publicarBase(item: any) {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Programar publicación ',
        bodyText: `Desea programar la publicación de la convocatoria: ${
          this.helperService.form1.get('nombreConcurso').value
        }`,
        rutaImagen: 'assets/images/icons/send.png',
        textCancel: 'No',
        textOk: 'Sí',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.basesService
          .publicarBase({
            movimientoId: null,
            baseId: item.baseId,
            entidadId: this.authenticationRepository.getCurrentUserValue
              .entidadId,
            estadoOldId: item.etapaId,
            estadoNewId: this.helperService.etapasRegistro.find(
              (del) => del.codProg === this.const.ETA_BASE_POR_PUBLICAR
            ).maeDetalleId,
          })
          .subscribe(
            (rest) => {
              if (rest) {
                this.toastService.showToast(
                  'Base Publicada correctamente',
                  'success'
                );
              }
              this.helperService.initializeValues();
              this.router.navigateByUrl('pages/gestionbases');
            },
            (error) => this.toastService.showToast(error.message, 'danger')
          );
      } else {
        this.helperService.initializeValues();
        this.router.navigateByUrl('pages/gestionbases');
      }
    });
  }

  publicarBaseSuperAdmin(idBase: number, idEtapaActual: number) {

    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Programar publicación ',
        bodyText: `Desea programar la publicación de la convocatoria: ${
          this.helperService.form1.get('nombreConcurso').value
        }`,
        rutaImagen: 'assets/images/icons/send.png',
        textCancel: 'No',
        textOk: 'Sí',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.basesService
          .publicarBase({
            movimientoId: null,
            baseId: idBase,
            entidadId: this.authenticationRepository.getCurrentUserValue
              .entidadId,
            estadoOldId: idEtapaActual,
            estadoNewId: this.helperService.etapasRegistro.find(
              (del) => del.codProg === this.const.ETA_BASE_POR_PUBLICAR
            ).maeDetalleId,
          })
          .subscribe(
            (rest) => {
              if (rest) {
                this.toastService.showToast(
                  'Base Publicada correctamente',
                  'success'
                );
              }
              this.helperService.initializeValues();
              this.router.navigateByUrl('pages/gestionbases');
            },
            (error) => this.toastService.showToast(error.message, 'danger')
          );
      } else {
        this.helperService.initializeValues();
        this.router.navigateByUrl('pages/gestionbases');
      }
    });
  }

  nextStep() {
    window.scrollTo(0, 0);
    let form = null;
    switch (this.helperService.indexStepper) {
      case 0:
        form = this.helperService.form1;
        form.markAllAsTouched();
        this.saveOrUpdateForm1(form);
        this.helperService.perfiles = this.helperService.perfiles.filter(
          (item) =>
            this.helperService.jerarquiaSelected.regimen.codProg ===
            item.codProg
        );
        break;
      case 1:
        form = this.helperService.form2;
        form.markAllAsTouched();
        this.saveOrUpdateForm2(form);
        break;
      case 2:
        form = this.helperService.form3;
        form.markAllAsTouched();
        this.saveOrUpdateForm3(form);
        break;
      case 3:
        form = this.helperService.form4;
        form.markAllAsTouched();
        this.saveOrUpdateForm4(form);
        break;
      case 4:
        this.saveOrUpdateForm5();
        break;
      case 5:
        form = this.helperService.form6;
        form.markAllAsTouched();
        this.saveOrUpdateForm6(form);
        break;
    }
  }

  validVacantes(numeroVacantes: number, vacantes: any[]) {
    let c: number = 0;
    vacantes.forEach((item: any) => {
      c += item.nroVacante;
    });

    return numeroVacantes < c;
  }

  saveOrUpdateForm1(form: FormGroup) {
    if (form.invalid) return;
    if (
      form.pristine &&
      this.helperService.idStep1 &&
      !form.controls.baseSeleccionadaEsp.touched
    ) {
      this.helperService.indexStepper = 1;
    } else {
      const body = this.helperService.form1.getRawValue();
      const jerarquia = this.helperService.jerarquiaSelected;
      body.regimen = jerarquia.regimen.maeDetalleId;
      body.modalidad = jerarquia.modalidad.maeDetalleId;
      body.tipo = jerarquia.tipo.maeDetalleId;

      if (
        this.validVacantes(
          Number(body.numeroVacantes),
          this.helperService.form2.controls.vacantes.value
        )
      ) {
        form.controls['numeroVacantes'].setValue(
          this.helperService._nroVacantes
        );
        this.toastService.showToast(
          'El número de vacantes es menor a las vacantes ya creadas',
          'warning',
          'Atención'
        );
        return;
      }

      this.basesService
        .saveOrUpdateStep1(body, this.helperService.idStep1)
        .subscribe(
          (res) => {
            !this.helperService.idStep1
              ? this.toastService.showToast(
                  'Base creada correctamente',
                  'primary'
                )
              : this.toastService.showToast(
                  'Cambios guardados correctamente',
                  'primary'
                );
            this.helperService.setStepOne(res);
            this.basesService
              .getListaDeEvaluaciones(res.baseId)
              .subscribe((resLista) => {
                console.log("this.helperService.listaDeEvaluaciones :",this.helperService.listaDeEvaluaciones)

                console.log("this.helperService.listaDeEvaluaciones :",this.helperService.listaDeEvaluaciones)
                this.helperService.listaDeEvaluaciones = resLista;
              });

            setTimeout(() => {
              this.helperService.indexStepper = 1;
            }, 0);
          },
          (err) => {
            this.toastService.showToast(err.message || 'Erro al crear la base', 'danger');
          }
        );
    }
  }

  saveOrUpdateForm2(form: FormGroup) {
    if (form.invalid) {
      this.toastService.showToast(
        'Tiene que ingresar al menos un perfil para continuar',
        'danger'
      );
      return;
    }
    this.helperService.indexStepper = 2;
  }

  saveOrUpdateForm3(form: FormGroup) {
    if (form.invalid) return;
    if (
      form.pristine &&
      this.helperService.idStep3 &&
      form.controls.declaraJuradaRequeridosToDelete.value.length === 0
    ) {
      this.helperService.indexStepper = 3;
    } else {
      const body = this.helperService.form3.getRawValue();
      this.basesService
        .saveOrUpdateStep3(
          body,
          this.helperService.idBase,
          this.helperService.idStep3
        )
        .subscribe(
          (res) => {
            !this.helperService.idStep3
              ? this.toastService.showToast(
                  'Etapa añadida correctamente',
                  'primary'
                )
              : this.toastService.showToast(
                  'Cambios guardados correctamente',
                  'primary'
                );
            this.helperService.setStepThree(res);
            setTimeout(() => {
              this.helperService.indexStepper = 3;
            }, 0);
          },
          (err) => this.toastService.showToast(err, 'danger')
        );
    }
  }

  saveOrUpdateForm4(form: FormGroup) {
    let fil = this.helperService.listaDeEvaluaciones.filter(
      (obj) => obj.informeDetalleId === null
    );

    if (fil.length !== 0) {
      this.toastService.showToast(
        'Debe registrar todos los informes, no se guardaron los últimos cambios',
        'warning'
      );
      return;
    }

    if (form.invalid) {
      return;
    }
    if (form.pristine && !this.helperService.cambiosEvaluacion) {
      this.helperService.indexStepper = 4;
    } else {
      const params = [];
      this.helperService.listaDeEvaluaciones.forEach((element) => {
        params.push({
          evaluacionDetalleId: element.evaluacionDetalleId,
          evaluacionEntidadId: element.evaluacionEntidadId,
          informeDetalleId: element.informeDetalleId,
          baseEvaluacionId: element.baseEvaluacionId,
          estado: '1',
          peso: element.peso,
          puntajeMinimo: element.puntajeMinimo,
          puntajeMaximo: element.puntajeMaximo,
        });
      });
      this.basesService
        .saveOrUpdateStep4(
          this.helperService.idBase,
          form.controls.observacion.value,
          params
        )
        .subscribe(() => {
          this.toastService.showToast(
            'Cambios guardados correctamente',
            'primary'
          );
          this.basesService
            .getListaDeEvaluaciones(this.helperService.idBase)
            .subscribe((res) => {
              this.helperService.listaDeEvaluaciones = res;
              let p = 0,
                min = 0,
                max = 0;
              this.helperService.listaDeEvaluaciones.forEach(function (num) {
                p += num.peso;
                min += (num.puntajeMinimo * num.peso) / 100;
                max += (num.puntajeMaximo * num.peso) / 100;
              });
              this.helperService.peso = p;
              this.helperService.punMin = min;
              this.helperService.punMax = max;
            });
          this.helperService.cambiosEvaluacion = false;
          setTimeout(() => {
            this.helperService.indexStepper = 4;
          }, 0);
          this.helperService.idStep4 = this.helperService.idBase;
        });
    }
  }

  saveOrUpdateForm5() {
    if (this.helperService.listaCronogramas.length > 0) {
      let lista = null;
      lista = this.helperService.etapas.find((valor) => (valor.codProg = '1'));

      let tieneDifucion = false;
      for (let item of this.helperService.listaCronogramas) {
        if (item.etapaId === lista.value) {
          tieneDifucion = true;
          break;
        }
      }
      if (tieneDifucion) {
        setTimeout(() => {
          this.helperService.indexStepper = 5;
        }, 0);
        this.helperService.idStep5 = this.helperService.idBase;
      } else {
        this.toastService.showToast(
          'Debe añadir un cronograma de Etapa Difusión como mínimo.',
          'danger'
        );
        return;
      }
    }
  }

  procesaForm6(form: FormGroup): boolean {
    console.log("this.helperService.estadoBase:",this.helperService)
    if (
      this.helperService.estadoBase === this.const.ETA_BASE_POR_REVISAR ||
      this.helperService.estadoBase === this.const.ETA_BASE_REVISADO ||
      this.helperService.estadoBase === this.const.ETA_BASE_POR_PUBLICAR
    ) {
      this.saveObservaciones();
      return;
    }
    if (form.invalid) return;
    if (form.pristine) {
      this.toastService.showToast(
        'No se ha realizado cambio alguno',
        'primary'
      );
      this.helperService.initializeValues();
      this.router.navigateByUrl('pages/gestionbases');
    } else {
      const informes = [];
      form.controls.informes.value.forEach((element) => {
        informes.push({
          tipoInformeId: element.tipoInformeId,
          idInforme: element.value,
        });
      });
      // this.changeStateRevisado();
      // return; 
      this.basesService
        .saveOrUpdateStep6(
          this.helperService.idBase,
          form.controls.observacion.value,
          informes
        )
        .subscribe((res) => {
          if (res.status.success) {
            this.toastService.showToast(res.payload.resultado, 'success');
            console.log("this.authenticationRepository.isSuperAdminEntidad():",this.authenticationRepository.isSuperAdminEntidad())

            if (this.authenticationRepository.isSuperAdminEntidad()) {
              console.log("entre res.payload.resultado:",res.payload.idBase)
              this.changeStateRevisado(res.payload.idBase);
            } else {
              this.helperService.initializeValues();
              this.router.navigateByUrl('pages/gestionbases');
            }
          } else {
            this.toastService.showToast(res.status.error.messages[0], 'danger');
          } 
        });
    }
  }

  changeStateRevisado(idBase:number) {
    
    let obs = [...this.helperService.observaciones];
    obs = obs.filter((o) => o.description && o.resuelto === false);
    const observaciones = obs.map((o) => {
      return {
        etapa: o.step + 1,
        observacion: o.description,
      };
    });

    const idEtapaActual = this.helperService.etapasRegistro.find(
      (eta) => eta.codProg === this.const.ETA_BASE_POR_REVISAR
    ).maeDetalleId;
    const idEtapaNueva = this.helperService.etapasRegistro.find((eta) => {
      return eta.codProg === this.const.ETA_BASE_REVISADO;
  }).maeDetalleId;

    console.log("idEtapaActual",idEtapaActual)
    console.log("idEtapaNueva",idEtapaNueva)
    
    this.basesService
    .saveMovimientosObservaciones(
      idEtapaActual,
      idEtapaNueva,
      idBase,
      observaciones
    )
    .subscribe((res) => {
      if (observaciones.length > 0) {
        this.helperService.initializeValues();
        this.router.navigateByUrl('pages/gestionbases');
      } else {
        const modal = this.dialog.open(BaseRevisadaModalComponent);
        modal.afterClosed().subscribe((rest) => {
          this.publicarBaseSuperAdmin(idBase, idEtapaNueva);
        });
      }
    });
    
  } 

  saveOrUpdateForm6(form: FormGroup) {
    if (this.authenticationRepository.isGestor()) {
      if (form.invalid) {
        this.toastService.showToast('No se ha realizado cambio alguno', 'primary' );
        return;
      }

      const confirmModal = this.dialog.open(ModalConfirmationComponent, {
        data: {
          title: ' ',
          bodyText: '¿Desea enviar al Coordinador para su Revisión?',
          rutaImagen: ' '
        },
      });
      confirmModal.afterClosed().subscribe((res) => {
        if (res) {
          this.procesaForm6(form);
        }
      });
    } else {
      this.procesaForm6(form);
    }
  }

  async canDeactivate() {
    if (this.helperService.jerarquiaSelected) {
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

  previousStep() {
    window.scrollTo(0, 0);
    this.helperService.indexStepper--;
  }

  change(e) {
    window.scrollTo(0, 0);
    this.helperService.indexStepper = e.selectedIndex;
    this.helperService.form1.controls['numeroVacantes'].setValue(
      this.helperService._nroVacantes
    );
  }

  changeMode(mode: number) {
    this.mode = mode;
  }

  onResized(e) {
    this.bodySize = e.newWidth + 'px';
  }

  getValidateActiveSave(): boolean {
    return false;
  }
}
