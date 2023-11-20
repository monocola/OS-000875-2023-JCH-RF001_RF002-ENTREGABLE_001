import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MaestraParametro } from '../../../@data/model/maestra-parametro';
import { MaestraParametroRepository } from '../../../@domain/repository/maestra-parametro.repository';
import { forkJoin } from 'rxjs';
import { CicloService } from '../../../@data/services/ciclo.service';
import { AuthenticationRepository } from '../../../@domain/repository/authentication.repository';
import { ToastService } from '../../@common-components/toast';
import { Cronograma } from '../../../@data/model/cronograma';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { ModalRegistrarActComponent } from './modal-registrar-act/modal-registrar-act.component';
import { MatDialog } from '@angular/material/dialog';
import { ModalResolucionComponent } from './modal-resolucion/modal-resolucion.component';
import { Router } from '@angular/router';
import { ModalRemoveCronogramaComponent } from './modal-remove-cronograma/modal-remove-cronograma.component';
import { CronogramaRepository } from '../../../@domain/repository/cronograma.repository';
import { FileVisualizerComponent } from '../../@common-components/file-visualizer/file-visualizer.component';
import { UtilRepository } from '../../../@domain/repository/util.repository';
import { base64ToFilePromise } from 'src/app/utils/converterFile';
import * as FileSaver from 'file-saver';
import moment from 'moment';

@Component({
  selector: 'gdr-web-cronograma',
  templateUrl: './cronograma.component.html',
  styleUrls: ['./cronograma.component.scss'],
})
export class CronogramaComponent implements OnInit {
  searchMode = false;
  dataToEdit: any = null;
  dataPDF = null;
  estados: any[];
  tabResoluciones = false;
  fileBase64 = null;
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));

  constructor(
    private fb: FormBuilder,
    private maeParametroRepository: MaestraParametroRepository,
    private cicloService: CicloService,
    private authenticationService: AuthenticationRepository,
    private toastService: ToastService,
    private dialog: MatDialog,
    private router: Router,
    private cronogramaRepository: CronogramaRepository,
    private util: UtilRepository
  ) {}

  frm: FormGroup = null;
  cicloDefaultDesc;
  cicloDefault;
  etapa: MaestraParametro[];
  responsable: MaestraParametro[];
  profile = this.authenticationService.getCurrentUserValue;
  holderText =
    'Buscar por Etapa, actividades, fecha de inicio, fecha fin, responsable';
  holderText2 =
    'Buscar por Nombre, fecha de registro, fecha de aprobación, responsable';
  lstCronograma: Cronograma[] = [];
  lstResolucion: Cronograma[] = [];
  ordersTableColumns: TableColumn[];
  flagHabilitar = true;
  ordersTableColumns2: TableColumn[];
  anio: any[] = [];

  ngOnInit(): void {
    this.setCiclo();
    this.loadCombox();
    this.initForm();
    this.initializeColumns();
    this.initializeColumns2();
    this.getResolucion();
    this.getCronogramas();
  }

  loadCombox() {
    const getEtapa = this.maeParametroRepository.getMaestraParametro(
      'ETAPA_GDR'
    );
    const getResponsable = this.maeParametroRepository.getMaestraParametro(
      'RESPONSABLE_GDR'
    );
    forkJoin([getEtapa, getResponsable]).subscribe(
      (results) => {
        this.etapa = results[0];
        this.responsable = results[1];
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  initForm() {
    this.frm = this.fb.group({
      etapa: '',
    });
  }

  initializeColumns() {
    this.ordersTableColumns = [
      { name: 'N°', dataKey: 'etapaId', position: 'left', isSortable: true },
      {
        name: 'Etapas',
        dataKey: 'descripcionEtapa',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Actividades',
        dataKey: 'descripcionActividad',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Fecha de inicio',
        dataKey: 'fechaInicio',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Fecha fin',
        dataKey: 'fechaFin',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Responsable',
        dataKey: 'responsable',
        position: 'left',
        isSortable: true,
      },
    ];
  }

  initializeColumns2() {
    this.ordersTableColumns2 = [
      { name: 'N°', dataKey: 'numeracion', position: 'left', isSortable: true },
      {
        name: 'Nombre',
        dataKey: 'descripcionResolucion',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Fecha de registro',
        dataKey: 'fechaCreacion',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Fecha de aprobación',
        dataKey: 'fechaAprobacion',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Responsable',
        dataKey: 'descripcionResponsable',
        position: 'left',
        isSortable: true,
      },
    ];
  }

  setCiclo() {
    if (this.ciclo.length !== 0 && this.ciclo) {
      this.cicloDefaultDesc = ' - ' + this.ciclo.anio;
      this.cicloDefault = this.ciclo.cronogramaId;
    } else {
      this.cicloDefaultDesc = '';
      this.cicloDefault = 0;
    }
  }

  clear() {
    this.loadCombox();
    this.initForm();
    this.initializeColumns();
    this.getCronogramas();
  }

  getCronogramas() {
    this.searchMode = true;
    const body = this.frm.getRawValue();
    if (this.ciclo.length !== 0 && this.ciclo) {
      this.cronogramaRepository.getCronogramas(this.ciclo, body).subscribe(
        (res: any) => {
          let response = res;
          this.lstCronograma = response.items;
         
          this.lstCronograma =this.lstCronograma.sort((a, b) => new Date(a.fechaInicio).getTime() - new Date(b.fechaInicio).getTime());
          this.lstCronograma =this.lstCronograma.sort((a, b) => a.etapaId- b.etapaId);
          this.lstCronograma.forEach((item: any) => {
            item.descripcionEtapa = item.descripcionEtapa.toUpperCase()
            if (item.fechaInicio) {
              item.fechaInicio = moment(item.fechaInicio).format(
                'DD[/]MM[/]YYYY'
              );
            }
            if (item.fechaFin) {
              item.fechaFin = moment(item.fechaFin).format('DD[/]MM[/]YYYY');
            }
          });

          this.flagHabilitar = response.flagEdicion !== '1';
          if (this.lstCronograma.length <= 0) {
            this.toastService.showToast(
              'No se encontraron resultados',
              'primary'
            );
          }
        },
        (err) => {
          this.toastService.showToast(err.message, 'danger');
        }
      );
    } else {
      this.toastService.showToast(
        'Para mostrar el cronograma debe registrar un ciclo',
        'danger'
      );
    }
  }

  downloadCronograma() {
    if (this.ciclo.length !== 0 && this.ciclo) {
      let anio = this.ciclo.anio;
      let cronogramaId = this.ciclo.cronogramaId;
      this.cronogramaRepository.downloadExcel(cronogramaId, anio).subscribe(
        (res) => {
          const nameFile = `Cronograma Institucional ` + anio + `.xlsx`;
          const rutaFile = `data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,${res}`;
          base64ToFilePromise(rutaFile, nameFile, '').then((file) =>
            FileSaver.saveAs(file)
          );
        },
        (err) => this.toastService.showToast(err, 'danger')
      );
    } else {
      this.toastService.showToast(
        'Para mostrar el cronograma debe registrar un ciclo',
        'danger'
      );
    }
  }

  getResolucion() {
    this.searchMode = true;
    if (this.ciclo.length !== 0 && this.ciclo) {
      console.log("RESOLUCION", this.ciclo)
      this.cronogramaRepository.getResoluciones(this.ciclo).subscribe(
        (res: any) => {
          if (res.status.success) {
            console.log(res.payload);
            this.lstResolucion = res.payload;
            if (this.lstResolucion.length <= 0) {
              this.toastService.showToast(
                'No se encontraron resultados',
                'primary'
              );
            }
          } else {
            // this.toastService.showToast(res.status.error.messages[0], 'danger');
          }
        },
        (err) => {
          this.toastService.showToast(err.message, 'danger');
        }
      );
    }
  }

  openModalRegister(createMode: boolean = true) {
    const registerDialog = this.dialog.open(ModalRegistrarActComponent, {
      data: {
        createMode,
        estados: this.estados,
        dataToEdit: this.dataToEdit,
        responsable: this.responsable,
      },
    });

    registerDialog.afterClosed().subscribe((res) => {
      this.dataToEdit = null;
      if (res) {
        this.lstCronograma = [];
        // this.initializeColumns();
        // this.getCronogramas();
        this.ngOnInit();
        this.getR();
      }
    });
  }

  uploadResolution() {
    if (this.validarCronogramasFecha ()) {
      this.toastService.showToast(
        'Es necesario completar la Fecha de Inicio y Fin de todas las actividades.',
        'danger'
      );
      return;
    }

    const resolucion = this.dialog.open(ModalResolucionComponent, {});
    resolucion.afterClosed().subscribe((any) => {
      if (any) {
        this.ngOnInit ();
        this.getR();
        this.getAnio();
      }
    });
  }

  getAnio(){
    const getListCiclos = this.cicloService.getListCiclo(
      this.profile.entidadId
    );
    forkJoin([getListCiclos]).subscribe(
      (results) => {
        this.anio = results[0];
    const newciclo = this.anio.find((item) => item.anio === this.ciclo.anio);
    this.ciclo = newciclo
    sessionStorage.setItem('ciclo', JSON.stringify(this.ciclo));
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
    
  }

  validarCronogramasFecha () {
    let invalido: boolean = false;
    console.log (this.lstCronograma);

    this.lstCronograma.forEach ((cronograma: any) => {
      if (cronograma.fechaFin === null || cronograma.fechaFin === undefined ||
        cronograma.fechaInicio === null || cronograma.fechaInicio === undefined) {
          invalido = true;
      }
    });

    return invalido;
  }

  getR () {
    this.cronogramaRepository.getResoluciones(this.ciclo).subscribe(
      (res: any) => {
        if (res.status.success) {
          console.log(res.payload);
          this.lstResolucion = res.payload;
          if (this.lstResolucion.length <= 0) {
            this.toastService.showToast(
              'No se encontraron resultados',
              'primary'
            );
          }
        } else {
          // this.toastService.showToast(res.status.error.messages[0], 'danger');
        }
      },
      (err) => {
        this.toastService.showToast(err.message, 'danger');
      }
    );
  }

  editCronograma(item: any) {
    this.dataToEdit = item;
    this.openModalRegister(false);
  }

  removeCronograma(item: any) {
    if (this.cicloDefault && item) {
      const confirmModal = this.dialog.open(ModalRemoveCronogramaComponent, {
        data: {
          bodyText:
            'Se eliminará la siguiente actividad ¿Estás seguro de realizar la siguiente acción?',
        },
      });
      confirmModal.afterClosed().subscribe((resp) => {
        if (resp === true) {
          this.cronogramaRepository
            .deleteCronograma(this.cicloDefault, item.actividadId)
            .subscribe(() => {
              this.toastService.showToast(
                'Se realizó la eliminación exitosamente',
                'success'
              );
              this.getCronogramas();
            });
        }
      });
    }
  }

  actionShowPDF(events) {
    this.dataPDF = this.util.obtenerPdf(events.uuid).subscribe((res) => {
      this.fileBase64 = res;
      this.dialog.open(FileVisualizerComponent, {
        data: {
          base64String: this.fileBase64,
          filename: events.etapa,
          extension: 'pdf',
        },
      });
    });
  }

  onTabChanged(index: number) {
    this.tabResoluciones = index > 0;
    if (index === 0) {
      return;
    } else if (index === 1) {
      if (this.lstResolucion.length <= 0) {
        this.getResolucion();
      }
    }
  }

  hablitarEdicion() {
    if (this.ciclo.cronogramaId) {
      const cronogramaId = this.ciclo.cronogramaId;
      let flag;
      if (this.flagHabilitar) {
        flag = '1';
      } else {
        flag = '0';
      }
      this.cronogramaRepository
        .habilitaEdicion(cronogramaId, flag)
        .subscribe((res: any) => {
          if (!res.status.success) {
            this.toastService.showToast(res.status.error.messages[0], 'danger');
          } else {
            this.toastService.showToast(
              'Se realizó la habilitación exitosamente',
              'success'
            );
          }
          this.getCronogramas();
        });
    } else {
      this.toastService.showToast(
        'Para mostrar el cronograma debe registrar un ciclo',
        'danger'
      );
    }
  }
}
