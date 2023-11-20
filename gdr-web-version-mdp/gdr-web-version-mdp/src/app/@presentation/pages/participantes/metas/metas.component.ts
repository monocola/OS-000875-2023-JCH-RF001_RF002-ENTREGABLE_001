import { Component, OnInit, ViewChild } from '@angular/core';
import { MatPaginator, MatPaginatorIntl } from '@angular/material/paginator';
import { MatSort } from '@angular/material/sort';
import { MatTableDataSource } from '@angular/material/table';
import { MetaParticipante } from 'src/app/@data/model/meta';
import { IParticipanteEvaluador } from 'src/app/@data/model/participante';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { ServidoresRepository } from 'src/app/@domain/repository/servidores.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { TableColumn2 } from '../../../@common-components/material-table-check-select/table-column';

import { ModalDocumentoComponent } from '../modal-documento/modal-documento.component';
import { ValidarMetaComponent } from '../validar-meta/validar-meta.component';
import { ObservarMetaComponent } from '../observar-meta/observar-meta.component';

import { UtilRepository } from 'src/app/@domain/repository/util.repository';
import * as FileSaver from 'file-saver';
import { MaestraParametroRepository } from 'src/app/@domain/repository/maestra-parametro.repository';
import { forkJoin } from 'rxjs';
import { AuthenticationService } from 'src/app/@data/services/authentication.service';
import { ModalBtnHabilitarEdicionComponent } from '../modal-btn-habilitar-edicion/modal-btn-habilitar-edicion.component';
import { SelectionModel } from '@angular/cdk/collections';
import { getDutchPaginatorIntl } from '../../../@common-components/material-table-checkbox/paginator-translate';

import { MetaRepository } from '../../../../@domain/repository/meta.repository';
import { ModalEliminarMetaComponent } from '../modal-eliminar-meta/modal-eliminar-meta.component';




const ELEMENT_DATA: MetaParticipante[] = [];
// sessionStorage.setItem('userRoles', JSON.stringify(response.payload.roles));


@Component({
  selector: 'serv-talento-metas',
  templateUrl: './metas.component.html',
  styleUrls: ['./metas.component.scss'],
  providers: [
    { provide: MatPaginatorIntl, useValue: getDutchPaginatorIntl() }
  ]
})
export class MetasComponent implements OnInit {

  dataToDeleteMeta: MetaParticipante = null;
  ordersTableColumns: TableColumn2[];

  //  private userRoles = [];
  selection = new SelectionModel<IParticipanteEvaluador>(true, []);


  @ViewChild(MatPaginator) paginator: MatPaginator;

  @ViewChild(MatSort) sort: MatSort;
  fileBase64 = null;
  dataPDF = null;
  uiid = null;
  detUOId = null;
  personaId = null;
  nombreArchivo = null;
  listaEvaluados: IParticipanteEvaluador[] = [];
  participante: IParticipanteEvaluador = {};
  paginationSizes: number[] = [5, 10, 20, 50, 100];
  defaultPageSize = this.paginationSizes[0];
  dataSource = new MatTableDataSource<MetaParticipante>(ELEMENT_DATA);
  displayedColumns: string[] = [
    'numeracion',
    'indicador',
    'valorMeta',
    'peso',
    'descripcionEstado',
    'acciones'
  ];

  displayedColumns1: string[] = [
    'select',
    'numeracion',
    'indicador',
    'valorMeta',
    'peso',
    'descripcionEstado',
    'validadoEvaluador',
    //'acciones',

  ];
  pesoToTal: number;
  isPageable = true;

  estadoParaSubirDocumento = false;
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  cicloDefaultDesc;
  cicloDefault;
  base64String = '';
  filename = 'prueba';
  extensionFile = 'pdf';

  estados: any[];
  dataToEdit: any = null;
  rolId = null;

  isDisabled = false;

  constructor(
    private servidoresRepository: ServidoresRepository,
    private authService: AuthenticationRepository,
    private toastService: ToastService,
    public dialog: MatDialog,
    private router: Router,
    private util: UtilRepository,
    private maeParametroRepository: MaestraParametroRepository,
    private authenticationService: AuthenticationService,
    private metaRepository: MetaRepository,


  ) {
    this.participante = JSON.parse(
      sessionStorage.getItem('selected_participante')
    );
  }

  ngOnInit(): void {
    this.rolView();
    this.loadCombox();
    this.setCiclo();

    if (this.ciclo?.cicloId) {
      if (this.participante.accedioDirecto === true) {
        this.getListaParticipantes();
      } else {
        this.listarMetas();
      }
    } else {
      this.toastService.showToast(
        'No se tiene seleccionado ningún ciclo',
        'danger'
      );
    }
  }

  rolView() {
    this.authenticationService.getApplicationRoles().subscribe(item => {
      this.rolId = JSON.parse(sessionStorage.getItem('orden_rol_id'));
      console.info(this.rolId);
      });
  }

  loadCombox() {
    const getEstados = this.maeParametroRepository.getMaestraParametro('ESTADO_SERVIDOR_CIVIL_GDR');


    forkJoin([ getEstados]).subscribe(
      (results ) => {
        this.estados = results[0];
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  getListaParticipantes() {
    console.log('%cmetas.component.ts line:123 testttttttttt', 'color: #007acc;');
    console.log(this.authService.getCurrentUserValue.personaId);
    this.servidoresRepository.listarEvaluadosPersona(this.authService.getCurrentUserValue.personaId)
      .subscribe(listaEvaluados => {
        if (listaEvaluados.length === 0) {
          this.toastService.showToast(
            'No se encontraron evaluados correspondientes al usuario actual',
            'danger'
          );
          this.participante = {};
        } else {
          this.listaEvaluados = listaEvaluados;
          this.participante = this.listaEvaluados[0];
          this.detUOId = this.participante.detUnidadOrganicaId;
          this.personaId = this.participante.personaId;
        }
        console.log(this.participante );
        this.participante.accedioDirecto = true;
        this.listarMetas();
      },
        (err) => {
          this.toastService.showToast(err.message, 'danger');
        }
      );
  }

  listarMetas() {
    this.detUOId = this.participante.detUnidadOrganicaId;
    this.personaId = this.participante.personaId;
    this.servidoresRepository
      .listarMetasParticipante(
        this.participante.personaId,
        this.participante.detUnidadOrganicaId,
        this.ciclo?.cicloId ?? 0
      )
      .subscribe((x) => {
        console.log(x);
        x.listaMeta.forEach(element => {
          element.datoPeso = element.datoPeso+"%"
          if (element.tipoIndicadorProducto === "0" && element.tipoMeta === "0") {
            element.datoValorMeta = element.datoValorMeta+"%"
          }
          switch (element.estadoMeta) {
            case '1':
              element.colorEstadoMeta = '#FA9C0E';
              break;
            case '2':
              element.colorEstadoMeta = '#024487';
              break;
            case '3':
              element.colorEstadoMeta = '#e4520f';
              break;
            case '4':
              element.colorEstadoMeta = '#00BFA6';
              break;
            case '5':
              element.colorEstadoMeta = '#2D9CDB';
              break;
          }

          if (element.estadoMeta !== "2") {
            return this.estadoParaSubirDocumento = true;
          }
         
          
        
        });
        this.pesoToTal = x.pesoToTal;
        this.dataSource.data = x.listaMeta;

        this.dataSource.sort = this.sort;
        this.dataSource.paginator = this.paginator;

        this.uiid = x.uuid;
        this.nombreArchivo = x.nombreArchivo;

        this.dataSource.sort = this.sort;
        this.dataSource.paginator = this.paginator;
      });
  }

  setCiclo() {
    if (this.ciclo.length !== 0 && this.ciclo) {
      this.cicloDefaultDesc = ' - ' + this.ciclo.anio;
      this.cicloDefault = this.ciclo.cronogramaId;
    } else {
      this.cicloDefaultDesc = '';
      this.cicloDefault = 0;
      this.toastService.showToast(
        'No se encuentra ciclo configurado',
        'danger'
      );
    }
  }

  agregarMetas() {
    this.participante.pesoToTal = this.pesoToTal;
    sessionStorage.setItem('selected_participante', JSON.stringify(this.participante));
    this.router.navigate(['/pages/participantes/evaluados/metas/agregar-meta']);
  }

  uploadDocumento() {
    let dataEnvio = { participante: this.participante, ciclo: this.ciclo };
    const documento = this.dialog.open(ModalDocumentoComponent,
      { data: dataEnvio }
    );
    documento.afterClosed()
      .subscribe((any) => {
        if (any) {
          setTimeout(() => {
          this.ngOnInit();
            this.reseteo();
          }, 1000);
        }
      });
  }

  reseteo() {
    this.router.navigate(['pages/participantes/evaluados/metas']);
  }

  modalEliminarMeta(item: MetaParticipante, Boolean: true) {
    this.dataToDeleteMeta = item;
    this.dialog.open(ModalEliminarMetaComponent, {
      data: {
        bodyText: 'Se eliminará el siguiente registro ¿Estás seguro de realizar la siguiente acción?',
      },
    })
      .afterClosed()
      .subscribe(x => {
        if (x) {
          this.metaRepository.eliminarMeta(item.metaId)
            .subscribe(
              (deleted) => {
                if (deleted) {
                  this.toastService.showToast(
                    'Se realizó la eliminación exitosamente',
                    'success'
                  );
                  this.listarMetas();
                } else {
                  return false;
                }
              },
              (err) => this.toastService.showToast(err, 'danger')
            );
        }
      })
      ;
  }



  pageChangedEvaluado(page: number) {
    let listaEvaluadosPage = this.listaEvaluados.slice(page - 1, page);
    if (listaEvaluadosPage.length > 0) {
      this.participante = listaEvaluadosPage[0];
      this.participante.accedioDirecto = true;
      this.listarMetas();
    }
  }

  modalValidarMeta(element, validarMode: boolean) {
    let item = {
      trace: {
        traceId: "string"
      },
      payload: {
        metaId: element.metaId,
        rol:  this.rolId
      }
    };
      console.log('%cmetas.component.ts line:258 item', 'color: pink;', item);

    const validadarMetaDialog = this.dialog.open(ValidarMetaComponent, {

     data: {
        body: item
      },
    });

    validadarMetaDialog.afterClosed().subscribe((res) => {
      if (res) {
        this.listarMetas();
        console.log(res);
        // this.listarMetas();

      } /* else {
        this.toastService.showToast(
          'error',
          'danger'
        );
      } */
      });
  }

  validEnableSearchButtom() {
    return (this.rolId === 2 || this.rolId === 3) && this.pesoToTal !== 100;
  }

  validEnableSearchButtomSubirDoc() {
    return  (this.rolId === 2 || this.rolId === 3) && this.pesoToTal === 100 && this.estadoParaSubirDocumento !== true;
  }

  validEnableDownloadButtom() {
    return (this.pesoToTal === 0);
  }

  modalObservarMeta(element, validarMode: boolean) {
      let item = {
        trace: {
            traceId: "string"
          },
        payload: {
            metaId: element.metaId,
            descripcionObservacion : element.descripcionObservacion,
        }
      };

    const observarMetaDialog = this.dialog.open(ObservarMetaComponent, {
     data: {
        body: item
      },
    });

    observarMetaDialog.afterClosed().subscribe((res) => {
      if (res) {
      this.listarMetas();
        console.log(res);

      }/*  else {
        this.toastService.showToast(
          'error',
          'danger'
        );
      } */
      });
  }

  habilitar() {
   console.log(this.selection);
    if (this.selection.selected.length > 0 ) {

     console.log('click');
    }
    this.habilitarEdicion();

  }


  habilitarEdicion() {
     this.dialog.open(ModalBtnHabilitarEdicionComponent)
      .afterClosed()
      .subscribe((x) => {
      if (x) {
        this.selection.selected.forEach(element => {
          console.log(element.estadoMeta);
         // let selected = this.selection.selected.length > 0;
         /*  if (element.estadoMeta === "1" ) {
           selected === this.isDisabled;

          } */
          console.log(element, 'elementtttttttttt');
          const body = {

            trace: {
              traceId: "string"
            },
            payload: {
              meta: [
                {
                  metaId: element.metaId
                }
              ],
              cicloId: element.cicloId,
              rolId: this.rolId,
              detalleUoId: element.detaUoId
            }
          };
          console.log(body, 'body');

          this.metaRepository.updateEdicion(body).subscribe(
            result => {
              if (result.status.success) {
                this.toastService.showToast('Se habilitó la edición de manera exitosa', 'primary');
              } else {
                this.toastService.showToast('No se habilitó la edición de manera exitosa', 'danger');
              }
              this.listarMetas();
            },
            (err) => { this.toastService.showToast(err, 'danger'); }
          );

        });
        console.log(x);

      } /* else {
        this.toastService.showToast(
          'error',
          'danger'
        );
      } */
      });

  }

  actionDocumentoShowPDF() {

    let events = {
      uuid: this.uiid,
      nombre: this.nombreArchivo,
      detUOId: this.detUOId,
      personaId: this.personaId,
      cicloId: this.ciclo?.cicloId ?? 0

    };
    console.log(events, 'evnetsssssssssssssssss');
    if (events.uuid != null && events.uuid !== '') {
      this.dataPDF = this.util.obtenerDocumentoPdf(events.uuid)
        .subscribe(res => {
          this.fileBase64 = res;
          console.log(this.fileBase64);
          if (this.fileBase64) {
            this.base64String =
              'data:application/pdf;base64,' + this.fileBase64;
            FileSaver.saveAs(this.base64String, this.filename);
          } else {
            this.toastService.showToast(
              'Datos no encontrados',
              'danger');
          }
        });
    } else {
      this.dataPDF = this.util.obtenerDocumentoPdfSinUiid(events.detUOId, events.personaId, events.cicloId)
        .subscribe(res => {

          this.fileBase64 = res;
          if (this.fileBase64) {
            this.base64String =
              'data:application/pdf;base64,' + this.fileBase64;
            FileSaver.saveAs(this.base64String, this.filename);
          } else {
            this.toastService.showToast(
              'Datos no encontrados',
              'danger'
            );
          }
        });

    }
  }

  edit(metaId) {
    this.participante.pesoToTal = this.pesoToTal;
    sessionStorage.setItem('selected_participante', JSON.stringify(this.participante));
    this.router.navigateByUrl('/pages/participantes/evaluados/metas/' + metaId);
  }

  clear() {
/*     if (this.lstParticipantesCiviles.length !== 0) {
      this.lstParticipantesCiviles = [];
    }
    this.initForm();
    this.search();
    this.tipoOrganoChange(null); */
  }



  masterToggle() {
    if (this.isAllSelected()) {
      this.selection.clear();
      return;
    }
     this.selection.select(...this.dataSource._pageData(this.dataSource.data));
  }

  checkboxLabel(row?: IParticipanteEvaluador): string {
    if (!row) {
      return `${this.isAllSelected() ? 'deselect' : 'select'} all`;
    }
    return `${this.selection.isSelected(row) ? 'deselect' : 'select'} row ${row.estado + 1}`;
  }

  isAllSelected() {
    if (this.dataSource.data.length === this.selection.selected.length) {
      const numSelected = this.selection.selected.length;
      const numRows = this.selection.selected.length;
      return numSelected === numRows;
    } else {
      const numSelected = this.selection.selected.length;
      const numRows = this.dataSource.paginator.pageSize;
      return numSelected === numRows;
    }
  }

}
