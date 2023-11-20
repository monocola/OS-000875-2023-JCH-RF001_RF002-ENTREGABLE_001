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
import { DatePipe } from '@angular/common';

@Component({
  selector: 'serv-talento-historial-reuniones',
  templateUrl: './historial-reuniones.component.html',
  styleUrls: ['./historial-reuniones.component.scss']
})
export class HistorialReunionesComponent implements OnInit, AfterViewInit {
  @ViewChild(MatPaginator) paginator: MatPaginator;
  @ViewChild(MatSort) sort: MatSort;
  paginationSizes: number[] = [10, 20, 50, 100];
  defaultPageSize = this.paginationSizes[0];
  displayedColumns: string[] = ['select', 'numero', 'fecha', 'hora', 'duracion', 'estado', 'acciones'];
  dataSource = new MatTableDataSource<any>([]);
  holderText = 'Buscar por Nº, fecha, hora, duración, estado...';
  isFilterable = true;
  isPageable = true;
  dataToEdit: any = null;
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  // dataEvaluado = JSON.parse(sessionStorage.getItem('dataEvaluado'));
  cicloDefaultDesc: string;
  cicloDefault: number;
  selection = new SelectionModel(true, []);
  lstHistoryReuniones: any [] = [];
  searchMode = false;
  detalleUoId: number;
  evaluadoDetalleUoId: number;
  nombreEvaluado = '';
  showDownloadReport = true;

  constructor(
    private toastService: ToastService,
    private dialog: MatDialog,
    private reunionesRepository: ReunionesRepository,
    private activatedRoute: ActivatedRoute,
    // private configuracionRepository: ConfiguracionRepository,
    public datepipe: DatePipe
  ) {
    this.setCiclo();
    this.nombreEvaluado = sessionStorage.getItem('nombreEvaluado');
    this.valideShowDownloadReport();
  }

  ngOnInit(): void {
    this.detalleUoId = this.activatedRoute.snapshot.params['detalleUoId'];
    this.evaluadoDetalleUoId = this.activatedRoute.snapshot.params['evaluadoDetalleUoId'];
    this.search();
  }
  setCiclo() {
    if (this.ciclo.length !== 0 && this.ciclo) {
      this.cicloDefaultDesc = ' - ' + this.ciclo.anio;
      this.cicloDefault = this.ciclo.cicloId;
    } else {
      this.cicloDefaultDesc = '';
    }
  }

  valideShowDownloadReport() {
    let rol = JSON.parse(sessionStorage.getItem('roles'));
    if (rol.nombreRol.toUpperCase() !== "EVALUADO") {
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
    return `${this.selection.isSelected(row) ? 'deselect' : 'select'} row ${row.id + 1}`;
  }

  isAllSelected() {
    const numSelected = this.selection.selected.length;
    const numRows = this.dataSource.paginator?.pageSize;
    return numSelected === numRows;
  }


  colorEstado (estado) {
    let color = '#111111';
    switch (estado) {
      case '1' :
        color = '#0DACBC';
        break;
      case '2':
        color = '#FA9C0E';
        break;
      case '4' :
        color = '#8191A4';
        break;
      case '3' :
        color = '#EF2235';
        break;
    }
    return color;
  }
  nameEstado (estado) {
    let nombre = 'cancelado';
    switch (estado) {
      case '1' :
        nombre = 'Programada';
        break;
      case '2':
        nombre = 'Reprogramada';
        break;
      case '4' :
        nombre = 'Finalizada';
        break;
      case '3' :
        nombre = 'Cancelada';
        break;
    }
    return nombre;
  }



  openModalNotifacion(tipo?: string, elem?: any) {
    const registerDialog = this.dialog.open(ModalNotificacionesComponent, {
      data: {
        tipo: tipo
      },
    });
    registerDialog.afterClosed().subscribe((res) => {
      if (res) {
        if ( tipo === 'D' ) {
          /* delete */
          this.reunionesRepository.deleteReunion(elem.reunionId).subscribe(
            (res) => {
              if (res) {
                this.toastService.showToast('Se eliminó correctamente', 'primary');
                this.search();
              } else {
                this.toastService.showToast('No se puede eliminar', 'primary');
              }
            },
            (err) => {
              this.toastService.showToast(err.message, 'danger');
            }
          );

        } else if ( tipo === 'E' ) {
          /* email */
          // this.toastService.showToast('Se realizó notificación correctamente', 'primary');
        } else if ( tipo === 'N' ) {
          /* email Notification */
          //   this.configuracionRepository.notificaUsuarioReg("")
          //      .subscribe(response => {
          //         if (response.payload.estado) {
          //           this.toastService.showToast(response.payload.mensaje, 'success');
          //         } else {
          //           this.toastService.showToast(response.status.error.messages[0].message, 'danger');
          //         }
          //       });
           this.toastService.showToast('Se realizó notificación exitosamente', 'success');
           this.search();
        }
      }
    });
  }
  /** */
  search() {
    this.searchMode = true;
    let  body =  {
      detalleUoId: this.detalleUoId,
      cicloId: this.ciclo.cicloId,
      evaluadoDetalleUoId: this.evaluadoDetalleUoId,
    };
    this.reunionesRepository.listHistorialReuniones(body).subscribe(
      (res) => {
        console.log(res);
        this.lstHistoryReuniones = res;
        if (this.lstHistoryReuniones?.length === 0) {
          this.toastService.showToast('No se encontraron resultados', 'primary');
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
        bodyText: '¿Se eliminará la siguiente actividad ¿Estás seguro de realizar la siguiente acción?',
      },
    });
    registerDialog.afterClosed().subscribe((res) => {
      if (res === true ) {
        this.reunionesRepository.deleteReunion(item.reunionId)
          .subscribe(() => {
            this.toastService.showToast('Se realizó la eliminación exitosamente', 'success');
            this.search();
          });
      }
    });
  }


  setTableDataSource(data: any) {
    this.dataSource = new MatTableDataSource<any>(data);
    this.dataSource.paginator = this.paginator;
    this.dataSource.sort = this.sort;
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
  duracionString( dura: number) {
    if (dura) {
      let horas = Math.floor(dura / 60);
      let minutos = dura % 60;
      let horasStr = horas.toString().length === 1 ? '0' + horas : horas;
      let minutosStr = minutos.toString().length === 1 ? '0' + minutos : minutos;
      return horasStr + ':' + minutosStr;
    } else {
      return '--:--';
    }

  }
  openEditReunion(elem) {

    let fecha = new Date();
    let hora = (fecha.getHours().toString().length === 1 ?  '0'.concat(fecha.getHours().toString()) : fecha.getHours().toString()) + ":" + (fecha.getMinutes().toString().length === 1 ? '0'.concat(fecha.getMinutes().toString()) : fecha.getMinutes().toString()) + ":00";
    console.log("HORA ACTUAL", hora)
    console.log("HORA EDIT", elem.horaReunion)
    console.log("DATEPIPER",this.datepipe.transform(fecha, 'dd/MM/yyyy'))
    console.log("FECHA EDIT", elem.fechaReunion)
    var moment = require('moment');
    var momentA = moment(fecha,"DD/MM/YYYY HH:mm");
    var momentB = moment(elem.fechaReunion,"DD/MM/YYYY HH:mm");
    if (this.datepipe.transform(fecha, 'dd/MM/yyyy') == elem.fechaReunion) {
      if (hora >= elem.horaReunion) {
        this.toastService.showToast('La reunión ya empezó, no se puede editar', 'primary');
        return false;
      }
    } else {
      if (momentA > momentB) {
        this.toastService.showToast('La reunión ya empezó, no se puede editar', 'primary');
        return false;
      }
    }
    //if (hora >= elem.horaReunion  && this.datepipe.transform(fecha, 'dd/MM/yyyy') >= elem.fechaReunion) {
      //this.toastService.showToast('La reunión ya empezó, no se puede editar', 'primary');
      //return false;
    //}

    this.dataToEdit = elem;
    this.dataToEdit.cicloId = this.ciclo.cicloId;
    this.dataToEdit.detaUoId = this.detalleUoId;
    this.openFormReunion(false);
  }

  // openNewReunion() {
  //

  //   const elem = this.dataEvaluado;
  //   console.log("elem --->");
  //   console.log(elem);
  //   this.dataToEdit = this.dataEvaluado;
  //   this.dataToEdit.cicloId = this.ciclo.cicloId;
  //   this.dataToEdit.detaUoId = this.detalleUoId;
  //   this.openFormReunion(true);
  // }
  openFormReunion(createMode: boolean = true) {
    let  reprogramarMode: boolean = true;
    const registerDialog = this.dialog.open(FormReunionComponent, {
      data: {
        createMode,
        dataToEdit: this.dataToEdit,
        reprogramarMode
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
