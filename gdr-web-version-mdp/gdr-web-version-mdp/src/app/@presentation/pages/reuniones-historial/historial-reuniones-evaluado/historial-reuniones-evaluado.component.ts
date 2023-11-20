import { SelectionModel } from '@angular/cdk/collections';
import { AfterViewInit, Component, OnInit, ViewChild } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { MatPaginator } from '@angular/material/paginator';
import { MatSort } from '@angular/material/sort';
import { MatTableDataSource } from '@angular/material/table';
import { ActivatedRoute } from '@angular/router';
import { ReunionesRepository } from 'src/app/@domain/repository/reuniones.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { FormReunionComponent } from '../form-reunion/form-reunion.component';
import { ModalNotificacionesComponent } from '../modal-notificaciones/modal-notificaciones.component';
import { ModalEliminarReunionComponent } from '../modal-eliminar-reunion/modal-eliminar-reunion.component';
import { ConfiguracionRepository } from '../../../../@domain/repository/configuracion.repository';
import { Reuniones, TabsPuestos } from 'src/app/@data/model/reuniones';
import { forkJoin } from 'rxjs';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';

@Component({
  selector: 'serv-talento-historial-reuniones-evaluado',
  templateUrl: './historial-reuniones-evaluado.component.html',
  styleUrls: ['./historial-reuniones-evaluado.component.scss'],
})
export class HistorialReunionesEvaluadoComponent
  implements OnInit, AfterViewInit {
  @ViewChild(MatPaginator) paginator: MatPaginator;
  @ViewChild(MatSort) sort: MatSort;
  paginationSizes: number[] = [10, 20, 40, 110];
  defaultPageSize = this.paginationSizes[0];
  displayedColumns: string[] = [
    'select',
    'numero',
    'fecha',
    'hora',
    'duracion',
    'estado',
    'acciones',
  ];
  dataSource = new MatTableDataSource<any>([]);
  holderText = 'Buscar por fechas';
  isFilterable = true;
  isPageable = true;
  dataToEdit: any = null;
  dataToNew: any = null;
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  dataEvaluado = null;
  personaId: number = JSON.parse(sessionStorage.getItem('persona')).personaId;
  cicloDefaultDesc: string;
  cicloDefault: number;
  selection = new SelectionModel(true, []);
  lstHistoryReuniones: any[] = [];
  searchMode = false;
  detalleUoId: number;
  personaEvaluadorId: number;
  evaluadoDetalleUoId: number;
  evaluadoDTUOID: number;
  nombreEvaluado = '';
  showDownloadReport = true;
  listaPuestosTabs: TabsPuestos[] = [];
  puestoTab: TabsPuestos;
  lstReuniones: Reuniones[] = [];
  tieneEvaluador: boolean = false;
  entidad: number;
  persona: number;

  roleUsuario: boolean = true;

  constructor(
    private toastService: ToastService,
    private dialog: MatDialog,
    private reunionesRepository: ReunionesRepository,
    private activatedRoute: ActivatedRoute,
    private configuracionRepository: ConfiguracionRepository,
    private authenticationService: AuthenticationRepository
  ) {
    this.roleUsuario = JSON.parse(sessionStorage.getItem('roles')).rolId !== 13;

    this.setCiclo();
    this.nombreEvaluado = sessionStorage.getItem('nombreEvaluado');
    this.valideShowDownloadReport();
  }

  ngOnInit(): void {
    this.search();
    this.obtenerInformacionDetalleUO();
  }
  setCiclo() {
    if (this.ciclo.length !== 0 && this.ciclo) {
      this.cicloDefaultDesc = ' - ' + this.ciclo.anio;
      this.cicloDefault = this.ciclo.cicloId;
    } else {
      this.cicloDefaultDesc = '';
    }
  }

  searchEvaluados() {
    const body = {
      cicloId: this.ciclo.cicloId,
      uoId: this.puestoTab?.organigramaId,
      personaEvaluadorId: this.personaEvaluadorId,
      evaluadoDetalleUoId: this.puestoTab?.detalleUoId,
    };

    this.reunionesRepository.listReunionesEvaluado(body).subscribe(
      (res) => {
        console.log("resssssss", res)
        this.lstReuniones = res;
        if (this.lstReuniones?.length === 0) {
          this.toastService.showToast(
            'No se encontraron resultados',
            'primary'
          );
        } else{

        }
      },
      (err) => {
        this.toastService.showToast(err.message, 'danger');
      }
    );
  }

  buscarDetallePorEvaluado(lstReuniones) {
    const personaId = this.authenticationService.getCurrentUserValue.personaId;
    const resultado = lstReuniones.find(
      (reuniones) => reuniones.evaluadoPersonaId === personaId
    );
    console.log("AQUI ADENTRO", resultado)
    sessionStorage.setItem('dataEvaluado', JSON.stringify(resultado));
  }

  obtenerInformacionDetalleUO() {
    const getTabsPuestos = this.reunionesRepository.getPuestosPorEvaluadoTabs();
    this.searchMode = false;
    forkJoin([getTabsPuestos]).subscribe(
      (results) => {
        if (results.length > 0) {
          this.listaPuestosTabs = results[0];
          this.evaluadoDTUOID = results[0].evaluadoDetalleUoId
          this.puestoTab = this.listaPuestosTabs[0];
          if (this.listaPuestosTabs.length == 0) {
            this.tieneEvaluador = true;
          }
          this.detalleUoId = this.puestoTab.detalleUoId;
          this.entidad = this.puestoTab.entidadId
          this.persona = this.puestoTab.personaId
          this.evaluadoDTUOID = this.puestoTab.evaluadoDetalleUoId
          this.evaluadoDetalleUoId = this.puestoTab.detUnidadOrganicaId;
          this.personaEvaluadorId = this.puestoTab.personaId;
          //this.searchEvaluados();
        } else {
          this.tieneEvaluador = false;
          this.toastService.showToast(
            'No se encontró un evaluador asignado',
            'danger'
          );
        }
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  valideShowDownloadReport() {
    let rol = JSON.parse(sessionStorage.getItem('roles'));
    if (rol.nombreRol.toUpperCase() !== 'EVALUADO') {
      this.showDownloadReport = false;
    }
  }

  masterToggle() {
    if (this.isAllSelected()) {
      this.selection.clear();
      return;
    }
    this.selection.select(...this.dataSource._pageData(this.dataSource.data));
  }
  checkboxLabel(row?: any): string {
    if (!row) {
      return `${this.isAllSelected() ? 'deselect' : 'select'} all`;
    }
    return `${this.selection.isSelected(row) ? 'deselect' : 'select'} row ${
      row.id + 1
    }`;
  }

  isAllSelected() {
    const numSelected = this.selection.selected.length;
    const numRows = this.dataSource.paginator?.pageSize;
    return numSelected === numRows;
  }

  colorEstado(estado) {
    let color = '#111111';
    switch (estado) {
      case '1':
        color = '#0DACBC';
        break;
      case '2':
        color = '#FA9C0E';
        break;
      case '4':
        color = '#8191A4';
        break;
      case '3':
        color = '#EF2235';
        break;
    }
    return color;
  }
  nameEstado(estado) {
    let nombre = 'cancelado';
    switch (estado) {
      case '1':
        nombre = 'Programada';
        break;
      case '2':
        nombre = 'Reprogramada';
        break;
      case '4':
        nombre = 'Finalizada';
        break;
      case '3':
        nombre = 'Cancelada';
        break;
    }
    return nombre;
  }

  openModalNotifacion(tipo?: string, elem?: any) {
    const registerDialog = this.dialog.open(ModalNotificacionesComponent, {
      data: {
        tipo: tipo,
      },
    });
    registerDialog.afterClosed().subscribe((res) => {
      if (res) {
        if (tipo === 'D') {
          /* delete */
          this.reunionesRepository.deleteReunion(elem.reunionId).subscribe(
            (res) => {
              if (res) {
                this.toastService.showToast(
                  'Se eliminó correctamente',
                  'primary'
                );
              } else {
                this.toastService.showToast('No se puede eliminar', 'primary');
              }
            },
            (err) => {
              this.toastService.showToast(err.message, 'danger');
            }
          );
        } else if (tipo === 'E') {
          /* email */
          // this.toastService.showToast('Se realizó notificación correctamente', 'primary');
        } else if (tipo === 'N') {
          /* email Notification */
          let body = {
            cicloId: this.ciclo.cicloId,
            detaUoId: 0,
            evaluadoDetalleUoId: elem.evaluadoDetalleUoId,
            evaluadoPersonaId: elem.evaluadoPersonaId,
            fechaReunion: elem.fechaReunion,
            horaReunion: elem.horaReunion,
            duracion: elem.duracion,
            tipoAgendamientoId: elem.tipoAgendamientoId,
            enviaraNotificacion: 1,
            reunionId: elem.reunionId,
            esEvaluado: true,
            
          };
             this.reunionesRepository.notificarReunion(body)
                .subscribe(response => {
                  console.log("response", response)
                 });
          this.toastService.showToast(
            'Se realizó notificación exitosamente',
            'success'
          );
        }
        this.search();
      }
    });
  }
  /** */
  search() {
    this.searchMode = true;
    let body = {
      cicloId: this.ciclo.cicloId,
      personaId: this.personaId,
    };
    this.reunionesRepository.listHistorialReunionesEvaluados(body).subscribe(
      (res) => {
        console.log(res);
        this.lstHistoryReuniones = res;
        if (res == "No se encontraron registro del evaluado") {
          this.tieneEvaluador = true;
          this.toastService.showToast(
            'No se encontró un evaluador asignado',
            'danger'
          );
          this.lstHistoryReuniones = []
        }
        if (res == "No se encontraron registros de reuniones por evaluado") {
         // if (this.lstHistoryReuniones?.length === 0) {
            this.toastService.showToast(
              'No se encontraron resultados',
              'primary'
            );
         // } 
          this.lstHistoryReuniones = []
        }
        
        this.setTableDataSource(this.lstHistoryReuniones);
      },
      (err) => {
        this.toastService.showToast(err.message, 'danger');
      }
    );
  }
  /** datatable  */

  modalDeleteReunion(item: any) {
    console.log(item);
    const registerDialog = this.dialog.open(ModalEliminarReunionComponent, {
      data: {
        bodyText:
          '¿Se eliminará la siguiente actividad ¿Estás seguro de realizar la siguiente acción?',
      },
    });
    registerDialog.afterClosed().subscribe((res) => {
      if (res === true) {
        this.reunionesRepository.deleteReunion(item.reunionId).subscribe(() => {
          this.toastService.showToast(
            'Se realizó la eliminación exitosamente',
            'success'
          );
          this.search();
        });
      }
    });
  }

  setTableDataSource(data: any) {
    this.dataSource = new MatTableDataSource<any>(data);
    this.dataSource.paginator = this.paginator;
    this.dataSource.sort = this.sort;

    this.dataSource.filterPredicate = function (history, filterValue) {
      return history.fechaReunion
        .toLowerCase()
        .includes(filterValue.trim().toLowerCase());
    };
  }

  ngAfterViewInit(): void {
    this.dataSource.paginator = this.paginator;
  }

  applyFilter(event: Event) {
    const filterValue = (event.target as HTMLInputElement).value;

    this.dataSource.filter = filterValue.trim().toLowerCase();

    if (this.dataSource.paginator) {
      this.dataSource.paginator.firstPage();
    }
  }
  duracionString(dura: number) {
    if (dura) {
      let horas = Math.floor(dura / 60);
      let minutos = dura % 60;
      let horasStr = horas.toString().length === 1 ? '0' + horas : horas;
      let minutosStr =
        minutos.toString().length === 1 ? '0' + minutos : minutos;
      return horasStr + ':' + minutosStr;
    } else {
      return '--:--';
    }
  }
  openEditReunion(elem) {
    this.dataToEdit = elem;
    this.dataToEdit.cicloId = this.ciclo.cicloId;
    this.dataToEdit.detaUoId = this.detalleUoId;
    console.log("Entro EDITAAAR", this.dataToEdit)
    this.openFormReunion(false);
  }

  openNewReunion() {
    console.log("Entro nuevo")
    //this.dataEvaluado = JSON.parse(sessionStorage.getItem('dataEvaluado'));
    console.log("Entro nuevo2")
    this.dataToEdit = this.dataEvaluado;
    this.dataToNew = {
      detaUoId : this.detalleUoId,
      cicloId : this.ciclo.cicloId,
      evaluadoDetalleUoId: this.evaluadoDTUOID,
      evaluadoPersonaId: this.persona,
    };
    this.openFormReunion(true);
  }

  openFormReunion(createMode: boolean = true) {
    if (createMode) {
      this.dataToEdit = this.dataToNew
    }
    let reprogramarMode: boolean = true;

    const registerDialog = this.dialog.open(FormReunionComponent, {
      data: {
        createMode,
        dataToEdit: this.dataToEdit,
        reprogramarMode,
        lstHistoryReuniones: this.lstHistoryReuniones
      },
      width: '34.375rem',
    });
    registerDialog.afterClosed().subscribe((res) => {
      this.dataToEdit = null;
      if (res) {
        this.search();
      }
    });
  }
}
