import { SelectionModel } from '@angular/cdk/collections';
import { AfterViewInit, Component, OnInit, ViewChild } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { MatPaginator } from '@angular/material/paginator';
import { MatSort } from '@angular/material/sort';
import { MatTableDataSource } from '@angular/material/table';
import { Router } from '@angular/router';
import { forkJoin } from 'rxjs';
import { Reuniones, TabsPuestos } from 'src/app/@data/model/reuniones';
import { UnidadOrganicaCombo } from 'src/app/@data/model/unidadOrganicaCombo';
import { AuthenticationService } from 'src/app/@data/services/authentication.service';
import { OrganigramaRepository } from 'src/app/@domain/repository/organigrama.repository';
import { ReunionesRepository } from 'src/app/@domain/repository/reuniones.repository';
import { UnidadOrganicaRepository } from '../../../@domain/repository/unidad-organica.repository';
import { ToastService } from '../../@common-components/toast';
import { FormReunionComponent } from './form-reunion/form-reunion.component';
import { ModalNotificacionesComponent } from './modal-notificaciones/modal-notificaciones.component';

@Component({
  selector: 'serv-talento-reuniones-historial',
  templateUrl: './reuniones-historial.component.html',
  styleUrls: ['./reuniones-historial.component.scss'],
})
export class ReunionesHistorialComponent implements OnInit, AfterViewInit {
  @ViewChild(MatPaginator) paginator: MatPaginator;
  @ViewChild(MatSort) sort: MatSort;
  paginationSizes: number[] = [10, 20, 50, 100];
  defaultPageSize = this.paginationSizes[0];
  displayedColumns: string[] = [
    'descripcionEvaluado',
    'descripcionPuesto',
    'fecha',
    'hora',
    'duracion',
    'acciones',
  ];
  dataSource = new MatTableDataSource<Reuniones>([]);
  selection = new SelectionModel(true, []);
  holderText = 'Buscar por Evaluado, puesto, fecha, hora, duración ';
  isFilterable = true;
  isPageable = true;
  dataToEdit: any = null;
  showDownloadReport = false;

  filterForm: FormGroup = null;
  searchMode = false;
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  cicloDefaultDesc: string;
  cicloDefault: number;
  listaPuestosTabs: TabsPuestos[] = [];
  unidadOrganicaCbo: UnidadOrganicaCombo[];
  puestoTab: TabsPuestos;
  lstReuniones: Reuniones[] = [];

  constructor(
    private UnidadOrganicaRepository: UnidadOrganicaRepository,
    private toastService: ToastService,
    private dialog: MatDialog,
    private router: Router,
    private reunionesRepository: ReunionesRepository,
    private organigramaRepository: OrganigramaRepository,
    private authenticationService: AuthenticationService
  ) {
    let rolId = JSON.parse(sessionStorage.getItem('orden_rol_id'));

    if (rolId === 2) {
      this.redirectHistorialEvaluados();
      return;
    }

    // if(rolId == null) {

    //   this.authenticationService.getApplicationRoles().subscribe(item => {
    //     rolId = JSON.parse(sessionStorage.getItem('orden_rol_id'));
    //     if(rolId===2) {
    //       this.redirectHistorialEvaluados();
    //       return;
    //     }
    //   });

    // }

    if (rolId) {
      this.validarEvaluado(rolId);
    } else {
      this.authenticationService.getApplicationRoles().subscribe((item) => {
        rolId = JSON.parse(sessionStorage.getItem('orden_rol_id'));
        this.redirectHistorialEvaluados();
      });
    }

    this.valideShowDownloadReport();
  }

  redirectHistorialEvaluados() {
    this.router.navigate(['/pages/reuniones/historial']);
  }

  validarEvaluado(rolId: string) {
    if (rolId === '2') {
      const persona = JSON.parse(sessionStorage.getItem('persona'));
      this.organigramaRepository
        .getListPuesto(0, persona?.personaId)
        .subscribe((x) => {
          if (x && x.length > 0) {
            let puesto = x[0];

            sessionStorage.setItem('nombreEvaluado', persona?.nombreCompleto);
            this.router.navigate([
              '/pages/reuniones/' +
                puesto.detalleuoId +
                '/' +
                puesto.detUnidadOrganicaEvaluadorId,
            ]);
          } else {
            this.toastService.showToast(
              'No se ha encontrado puestos del evaluado',
              'danger'
            );
          }
        });
    }
  }

  valideShowDownloadReport() {
    let rol = JSON.parse(sessionStorage.getItem('roles'));
    if (rol.nombreRol.toUpperCase() !== 'EVALUADO') {
      this.showDownloadReport = true;
    }
  }

  ngOnInit(): void {
    this.loadCombox();
    this.initForm();
  }

  loadCombox() {
    const getUndOrganicaCbo = this.UnidadOrganicaRepository.getUnidadOrganicaCbo();
    const getTabsPuestos = this.reunionesRepository.getPuestosTabs();
    this.searchMode = false;
    forkJoin([getUndOrganicaCbo, getTabsPuestos]).subscribe(
      (results) => {
        this.unidadOrganicaCbo = results[0];
        this.listaPuestosTabs = results[1];
        this.puestoTab = this.listaPuestosTabs[0];
        this.setCiclo();
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }
  get f() {
    return this.filterForm.controls;
  }

  setCiclo() {
    if (this.ciclo.length !== 0 && this.ciclo) {
      this.cicloDefaultDesc = ' - ' + this.ciclo.anio;
      this.cicloDefault = this.ciclo.cicloId;
    } else {
      this.cicloDefaultDesc = '';
    }
  }

  initForm() {
    this.filterForm = new FormGroup({
      unidadOrganicaId: new FormControl(null, [Validators.required]),
      puestoId: new FormControl(null, [Validators.required]),
      fecha: new FormControl(null, [Validators.required]),
      hora: new FormControl(null, [Validators.required]),
      duracion: new FormControl(null, [Validators.required]),
    });
  }
  clear() {
    this.initForm();
    // this.search();
  }

  changeTab(index) {
    this.puestoTab = this.listaPuestosTabs[index];
    this.search();
  }

  search() {
    this.searchMode = true;
    const body = {
      cicloId: this.ciclo.cicloId,
      uoId: this.puestoTab?.organigramaId,
      personaEvaluadorId: this.puestoTab?.personaId,
      evaluadoDetalleUoId: this.puestoTab?.detalleUoId,
    };

    this.reunionesRepository.listReuniones(body).subscribe(
      (res) => {
        console.log(res);
        this.lstReuniones = res;
        if (this.lstReuniones?.length === 0) {
          this.toastService.showToast(
            'No se encontraron resultados',
            'primary'
          );
        }
        this.setTableDataSource(this.lstReuniones);
      },
      (err) => {
        this.toastService.showToast(err.message, 'danger');
      }
    );
  }
  excluir() {}
  activar() {}

  masterToggle() {
    if (this.isAllSelected()) {
      this.selection.clear();
      return;
    }
    this.selection.select(...this.dataSource._pageData(this.dataSource.data));
  }
  checkboxLabel(row?: Reunion): string {
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

  openEditReunion(elem) {
    this.dataToEdit = elem;
    this.dataToEdit.cicloId = this.ciclo.cicloId;
    this.dataToEdit.detaUoId = this.puestoTab.detalleUoId;
    this.openFormReunion(false);
  }

  openNewReunion(elem) {
    this.dataToEdit = elem;
    this.dataToEdit.cicloId = this.ciclo.cicloId;
    this.dataToEdit.detaUoId = this.puestoTab.detalleUoId;
    this.openFormReunion(true);
  }

  openFormReunion(createMode: boolean = true) {
    const registerDialog = this.dialog.open(FormReunionComponent, {
      data: {
        createMode,
        dataToEdit: this.dataToEdit,
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

  verHistorial(elem) {
    sessionStorage.setItem('nombreEvaluado', elem?.nombreEvaluado);
    sessionStorage.setItem('dataEvaluado', JSON.stringify(elem));
    this.router.navigate([
      '/pages/reuniones/' +
        elem?.evaluadoDetalleUoId +
        '/' +
        this.puestoTab?.detalleUoId,
    ]);
  }

  openModalNotifacion(tipo?: string) {
    const registerDialog = this.dialog.open(ModalNotificacionesComponent, {
      data: {
        tipo: tipo,
      },
    });
    registerDialog.afterClosed().subscribe((res) => {
      if (res) {
        this.search();
      }
    });
  }
  /** datatable  */

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
  get registrados() {
    let cant = 0;
    this.lstReuniones?.forEach((ele) => {
      if (ele.reunionId) {
        cant++;
      }
    });
    return cant;
  }
  get noRegistrados() {
    let cant = 0;
    this.lstReuniones?.forEach((ele) => {
      if (!ele.reunionId) {
        cant++;
      }
    });
    return cant;
  }
}

interface Reunion {
  id: number;
  numeroDocumento: string;
  puesto: string;
}
