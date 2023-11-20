import { Component, Inject, OnInit } from '@angular/core';
import { FormControl } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { EntityRequest } from 'src/app/@data/model/entityRequest';
import { AdministratorRepository } from 'src/app/@domain/repository/administrator.repository';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { MaestraEntidadRepository } from 'src/app/@domain/repository/maestra-entidad.repository';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { forkJoin } from 'rxjs';

@Component({
  selector: 'serv-talento-modal-verification',
  templateUrl: './modal-verification.component.html',
  styleUrls: ['./modal-verification.component.scss'],
})
export class ModalVerificationComponent implements OnInit {
  reasons: any[] = [];
  step = 0;
  reasonsSelected = new FormControl('');

  constructor(
    private parameterRepository: ParameterRepository,
    private administratorRepository: AdministratorRepository,
    public dialogRef: MatDialogRef<ModalVerificationComponent>,
    private toastService: ToastService,
    private router: Router,
    private authService: AuthenticationRepository,
    private maestraService: MaestraRepository,
    private maestraEntidadRepository: MaestraEntidadRepository,
    @Inject(MAT_DIALOG_DATA) public data: DataVerificationModel
  ) {}

  ngOnInit(): void {
    this.getReasonsToObserve();
  }

  onNoClick() {
    this.dialogRef.close();
  }

  getReasonsToObserve() {
    this.parameterRepository.getObservationReasons().subscribe(
      (res) => {
        this.reasons = res;
      },
      (err) => {
        this.toastService.showToast(err, 'danger');
      }
    );
  }

  nextStep(step: number) {
    switch (step) {
      case 1:
        if (!this.okEntity()) {
          this.step = 1;
        }
        break;
      case 2:
        this.step = 2;
        break;
      default:
        break;
    }
  }

  darAlta() {
    this.administratorRepository
      .updateSolicitudPersonaJuridica(
        this.data.entity.personaJuridica.solicitudPersonaId,
        this.data.entity.personaJuridica.ubigeoId,
        this.data.entity.personaJuridica.direccionCompleta,
        this.data.entity.personaJuridica.referenciaDireccion
      )
      .subscribe(
        (res) => {
          console.log (res);
          this.administratorRepository
            .approveEntity(this.data.entity.solicitudEntidad.solicitudEntidadId)
            .subscribe(
              (entity) => {
                console.log (entity);
                this.toastService.showToast(
                  'La solicitud ha sido aprobada con éxito',
                  'success'
                );
                this.onNoClick();
                if ( entity.status.success ) {
                  this.insConfigMaestra(entity.payload);
                }
                this.router.navigateByUrl('pages/gestionsolicitud');
              },
              (err) => {
                console.log (err);
                this.toastService.showToast(err, 'danger');
              }
            );
        },
        (err) => {
          console.log (err);
          this.toastService.showToast(err, 'danger');
        }
      );
  }

  async actualizarMaestrasEntidad(): Promise<boolean> {
    return await new Promise((resolve, reject) => {
      let listMaestraDetalles: any[] = [];
      const idCabecera: any = '';
      const entidadId: number = this.data.entity.solicitudEntidad
        .solicitudEntidadId;

      this.maestraService
        .getMaestraListDetail(idCabecera, entidadId)
        .toPromise()
        .then((res: any[]) => {
          res.forEach((item: any) => {
            if (item.listMaestraDetalles) {
              item.listMaestraDetalles.forEach((element: any) => {
                listMaestraDetalles.push(element);
              });
            }
          });

          let observables: any[] = [];

          for (let index = 0; index < listMaestraDetalles.length; index++) {
            const data = listMaestraDetalles[index];

            if (data.configuracionId) {
              observables.push(
                this.maestraEntidadRepository.actualizaConfigMaestraDetalle(
                  data.configuracionId,
                  true
                )
              );
            } else {
              observables.push(
                this.maestraEntidadRepository.asignaMaestraDetalleEntidad(
                  data.maeCabeceraId,
                  data.maeDetalleId,
                  entidadId
                )
              );
            }
          }

          forkJoin(observables).subscribe(
            (results) => {
              resolve(true);
            },
            (err) => {
              this.toastService.showToast(err, 'danger');
              resolve(false);
            }
          );
        })
        .catch((error: any) => {
          resolve(false);
        });
    });
  }

  observeRequest() {
    const reasons = this.reasonsSelected.value;
    this.administratorRepository
      .observeRequest(
        this.data.entity.solicitudEntidad.solicitudEntidadId,
        reasons
      )
      .subscribe(
        (res) => {
          this.toastService.showToast(
            'La solicitud ha sido observada con éxito',
            'success'
          );
          this.onNoClick();
          this.router.navigateByUrl('pages/gestionsolicitud');
        },
        (err) => this.toastService.showToast(err, 'danger')
      );
  }

  okEntity() {
    return (
      this.data.errorReniec || this.data.errorSunat || this.data.errorGenre
    );
  }

  insConfigMaestra(payload: any) {
    if ( payload != null && payload?.entidadId != null) {
        this.administratorRepository.insConfigMaestra(payload.entidadId)
          .subscribe((res) => {
            console.log("Config maestra de entidadId: "+ payload.entidadId+ " es " + res.status.success);
          }, (err) => console.log(err));
    }
  }
}

export class DataVerificationModel {
  entity: EntityRequest;
  errorReniec: boolean;
  errorSunat: boolean;
  errorGenre: boolean;
}
