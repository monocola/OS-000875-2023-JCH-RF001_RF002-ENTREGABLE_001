import { Component, OnInit, ViewChild } from '@angular/core';
import { NbDateService } from '@nebular/theme';
import { ParameterRepository } from '../../../@domain/repository/parameter.repository';
import { Ubigeo } from '../../../@data/model/ubigeo';
import { TypeDocument } from '../../../@data/model/typeDocument';
import { AdministratorRepository } from '../../../@domain/repository/administrator.repository';
import { SolicitudEntidad } from '../../../@data/model/solicitudesentidad';
import { concat, from } from 'rxjs';
import { map, toArray } from 'rxjs/operators';
import { ExportExcelModel } from '../../@service/export-excel.service';
import { Router } from '@angular/router';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { sortDataTableComponent } from 'src/app/utils/general';
import { Sort } from '@angular/material/sort';
import { FormControl } from '@angular/forms';
import moment from 'moment';
import { ToastService } from '../../@common-components/toast';

@Component({
  selector: 'serv-talento-administrador-solicitudes',
  templateUrl: './administrador-solicitudes.component.html',
})
export class AdministradorSolicitudesComponent implements OnInit {
  @ViewChild('autoInput') input;

  autoCompleateValues: string[] = [];
  activeBuscar: boolean = false;
  departamentos: Ubigeo[] = [];
  provincias: Ubigeo[] = [];
  distritos: Ubigeo[] = [];
  gobiernos: TypeDocument[] = [];
  sectores: TypeDocument[] = [];
  estados: TypeDocument[] = [];
  solicitudes: SolicitudEntidad[] = [];
  solicitudesAutocomplete: any[] = [];
  // ------------------ Values Form
  deptSelectValue: number = null;
  provSelectValue: number = null;
  distSelectValue: number = null;
  sectorSelectValue = 0;
  gobiernoSelectValue = 0;
  estadoSelectValue = 0;
  rucValue = new FormControl('');
  fromFechaValue = '';
  toFechaValue = '';

  min: Date;
  // max: Date;
  ordersTableColumns: TableColumn[];

  constructor(
    private parametrosRepository: ParameterRepository,
    private adminRepo: AdministratorRepository,
    protected dateService: NbDateService<Date>,
    private router: Router,
    private toast: ToastService
  ) {}

  ngOnInit(): void {
    this.initializeColumns();
    this.getDepartamentosData();
    this.getGobiernoData();
    this.getSectorData();
    this.getEstadosData();
    this.getDataGrilla();
  }

  // ---------------------------------- Para habilitar la tabla ---------------------------------- //

  initializeColumns() {
    this.ordersTableColumns = [
      {
        name: '#',
        dataKey: 'idSolicitudEntidad',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'FECHA/REG',
        dataKey: 'fechaRegistro',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'FECHA/ALTA',
        dataKey: 'fechaAlta',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'N° DE GOBIERNO',
        dataKey: 'nivelGobierno',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'SECTOR',
        dataKey: 'sector',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'RUC',
        dataKey: 'ruc',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'RAZÓN SOCIAL',
        dataKey: 'razonSocial',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'DPTO/PROV/DTTO',
        dataKey: 'lugarDep',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'ESTADO',
        dataKey: 'estadoSolicitud',
        position: 'left',
        isSortable: true,
      },
    ];
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.solicitudes);
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de entidades';
    model.headers = [
      'Fecha de registro',
      'Fecha de alta',
      'Fecha de baja',
      'Nivel de gobierno',
      'Sector',
      'RUC',
      'Razon Social',
      'Departamento',
      'Provicia',
      'Distrito',
      'Estado',
      'Tipo de documento Adminsitrador',
      'Nro doc. Administrador',
      'Nombres y apellidos',
      'Correo laboral',
      'Correo alterno',
      'Celular Alterno',
      'Celular',
      'Puesto',
    ];
    model.keys = [
      'fechaRegistro',
      'fechaAlta',
      'fechaBaja',
      'nivelGobierno',
      'sector',
      'ruc',
      'razonSocial',
      'departamento',
      'provincia',
      'distrito',
      'estadoSolicitud',
      'descripcionTipoDocumento',
      'numeroDocumento',
      'nombreCompleto',
      'correLaboral',
      'correoOpcional',
      'celularAlterno',
      'celular',
      'descripcionCargo',
    ];
    return model;
  }

  goDetail(e: SolicitudEntidad) {
    this.router.navigateByUrl(`pages/gestionsolicitud/${e.idSolicitudEntidad}`);
  }

  // ---------------------------------- Fin edicion tabla ---------------------------------- //

  private getDepartamentosData() {
    this.parametrosRepository.getDepartamento().subscribe((value) => {
      this.departamentos = value;
    });
  }
  private getGobiernoData() {
    this.parametrosRepository.getGobierno().subscribe((value) => {
      this.gobiernos = value;
    });
  }
  private getSectorData() {
    this.parametrosRepository.getSector().subscribe((value) => {
      this.sectores = value;
    });
  }
  private getEstadosData() {
    this.parametrosRepository.getEstadoSolicitud().subscribe((value) => {
      this.estados = value;
    });
  }

  cambioDept(idDept: number) {
    this.cleanProvincias();
    this.cleanDistritos();
    this.parametrosRepository.getProvincias(idDept).subscribe((value) => {
      this.provincias = value;
    });
  }

  cambioProv(idProv: number) {
    this.cleanDistritos();
    this.parametrosRepository.getDistritos(idProv).subscribe((value) => {
      this.distritos = value;
    });
  }

  getDataGrilla() {
    if (this.fromFechaValue !== '' && this.toFechaValue === '') {
      this.toast.showToast(
        'Elija la fecha fin para iniciar la búsqueda.',
        'warning',
        'Atención'
      );
      return;
    }
    this.adminRepo
      .getSolicitudesEntidaes(
        this.estadoSelectValue,
        this.gobiernoSelectValue,
        this.sectorSelectValue,
        this.rucValue.value?.razonSocial,
        this.rucValue.value?.ruc,
        this.fromFechaValue,
        this.toFechaValue,
        this.deptSelectValue,
        this.provSelectValue,
        this.distSelectValue
      )
      .subscribe((values) => {
        this.solicitudes = values;
        if (this.solicitudesAutocomplete.length === 0) {
          this.solicitudesAutocomplete = [];
          this.solicitudes.forEach((s) => {
            this.solicitudesAutocomplete.push({
              ruc: s.ruc,
              razonSocial: s.razonSocial,
              valueToShow: s.ruc + ' / ' + s.razonSocial,
            });
          });
        }
        this.procesarAutocomplete(this.solicitudes);
      });
  }

  limpiarFiltros() {
    this.cleanProvincias();
    this.cleanDistritos();
    this.resetDept();
    this.resetSector();
    this.resetGobierno();
    this.resetEstado();
    this.resetRuc();
    this.resetFromFecha();
    this.resetToFecha();
    this.getDataGrilla();
  }

  cleanDistritos() {
    this.distritos = [];
    this.distSelectValue = null;
  }

  cleanProvincias() {
    this.provincias = [];
    this.provSelectValue = null;
  }

  resetDept() {
    this.deptSelectValue = null;
  }

  resetSector() {
    this.sectorSelectValue = 0;
  }

  resetGobierno() {
    this.gobiernoSelectValue = 0;
  }

  resetEstado() {
    this.estadoSelectValue = 0;
  }

  resetRuc() {
    this.rucValue.patchValue('');
  }

  resetFromFecha() {
    this.fromFechaValue = '';
  }

  resetToFecha() {
    this.toFechaValue = '';
  }

  isvalidDateOrEmpty(fromFechaValue: string) {
    if (fromFechaValue != null && fromFechaValue.length !== 0) {
      return moment(fromFechaValue, 'DD/MM/YYYY').isValid();
    }
    return true;
  }
  isvalidDate(fromFechaValue: string) {
    if (fromFechaValue != null) {
      return moment(fromFechaValue, 'DD/MM/YYYY').isValid();
    }
    return false;
  }

  getEnabledSearch() {
    return this.validacionFechas();
  }

  private procesarAutocomplete(solicitudes: SolicitudEntidad[]) {
    if (this.autoCompleateValues.length === 0) {
      let ruc = from(solicitudes).pipe(
        map((value) => {
          return value.ruc;
        })
      );
      let razonsocial = from(solicitudes).pipe(
        map((value) => {
          return value.razonSocial;
        })
      );

      concat(ruc, razonsocial)
        .pipe(toArray())
        .subscribe((valores) => {
          this.autoCompleateValues = valores;
        });
    }
  }

  getMinToDate(fromFechaValue) {
    if (fromFechaValue != null && fromFechaValue.length !== 0) {
      return this.dateService.addDay(fromFechaValue, 1);
    }
    return null;
  }

  private validacionFechas() {
    //   if (this.fromFechaValue != null && this.fromFechaValue !== "") {
    //     if (this.toFechaValue != null && this.toFechaValue !== "") {
    //       return true;
    //     } else {
    //       return false;
    //     }
    //   } else return this.toFechaValue != null && this.toFechaValue !== "";
    return false;
  }
}
