import { EvaluacionCurricularRepository } from 'src/app/@domain/repository/evaluacion-curricular.repository';
import { ListaGestionNotasRepository } from 'src/app/@domain/repository/lista-gestion-notas.repository';
import { SeguimientoComunicadoService } from './../seguimiento-comunicado/seguimiento-comunicado.service';
import { Component, OnInit } from '@angular/core';
import { ComboitemModel } from '../../../@data/model/generic/comboitem.model';
import { FormBuilder, FormGroup } from '@angular/forms';
import { Router } from '@angular/router';
import { NbPosition, NbTrigger } from '@nebular/theme';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { Sort } from '@angular/material/sort';
import { sortDataTableComponent } from '../../../utils/general';
import { MatDialog } from '@angular/material/dialog';
import { ToastService } from '../../@common-components/toast';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { SeleccionServirRepository } from 'src/app/@domain/repository/seleccion-servir-repository';
import { ExportExcelModel } from '../../@service/export-excel.service';
import moment from 'moment';
@Component({
  selector: 'serv-talento-seguimiento-evaluacion',
  templateUrl: './seguimiento-evaluacion.component.html',
  styleUrls: ['./seguimiento-evaluacion.component.scss']
})
export class SeguimientoEvaluacionComponent implements OnInit {

  popHint: NbTrigger = NbTrigger.HINT;
  pospopRigth: NbPosition = NbPosition.END;
  today: moment.Moment = moment();
  fechaInicio: moment.Moment = moment();
  fechaFin: moment.Moment = moment();
  progressPorcent = 0;
  grupo: ComboitemModel[] = [];
  evaluacion: ComboitemModel[] = [];
  perfiles: ComboitemModel[] = [];
  estados: ComboitemModel[] = [];
  rangoPuntajes: any[] = [];


  filterForm: FormGroup = null;
  showCompleateFilter: boolean = false;
  // isConocimintoSelected: boolean = false;
  columns: TableColumn[];
  data: any[] = [];

  cantMaxPuntaje = 100;

  page: number = 0;
  size: number = 10;
  total: number = 0;
  envio: boolean = false;

  filtros = {
    perfilId: 0,
    notaMenor: '',
    notaMayor: '',
    estadoId: 0,
    fechaInicioPostulacion: '',
    fechaFinPostulacion: '',
  };

   convocatoria: string;
   etapaId: number;

   datosEtapa: any = {};

  getPaginado(e) {
    this.page = e.pageIndex;
    this.size = e.pageSize;
    this.getData();
  }

  constructor(
    private fb: FormBuilder,
    public router: Router,
    public dialog: MatDialog,
    private toast: ToastService,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    private listaGestionNotasRepository: ListaGestionNotasRepository,
    public helperService: SeguimientoComunicadoService,
    private seleccionService: SeleccionServirRepository,
    private listaGestionNotas: ListaGestionNotasRepository,
    private evaluacionCurricularRepository: EvaluacionCurricularRepository,
  ) { }

  get f() {
    return this.filterForm.controls;
  }

  get g() {
    return this.helperService.formConvocatoria.controls;
  }

  ngOnInit(): void {
    this.initializeForm();
    if (!this.helperService.formConvocatoria) {
      this.helperService.initializeForm();
    }
    this.convocatoria = this.g.nomConvocatoria.value;
    this.etapaId = this.g.etapaId.value;

    if (this.g.baseId.value === 0) {
      this.router.navigateByUrl('pages/seguimientoconvocatoria');
      return;
    }
    this.cargarPuntajes();
    this.obtenerListaFechas();
    this.obtenerPerfiles();
    this.obtenerEvaluaciones();
    this.obtenerEstados();
    this.iniciarColumnas();
  }

  iniciarColumnas() {
    this.columns = [];
  }

  obtenerListaFechas() {

    this.evaluacionConocimientosService.listarFechasEtapa(this.etapaId,this.g.baseId.value).subscribe((mc) => {
      this.datosEtapa = mc;
    });
  }

  irEvaluaciones() {
    sessionStorage.setItem('convocatoriaId', this.g.baseId.value);
    this.router.navigateByUrl('pages/lista-evaluaciones/lista-gestion-notas');
  }

  obtenerEstados() {
    this.evaluacionCurricularRepository.listarEstadoEvaluacionCurricular("ESTADO_CONOCIMIENTO").subscribe((mc) => {
      this.estados = mc;
    });
  }

  cargarPuntajes() {
    for (let index = 0; index <= this.cantMaxPuntaje; index++) {
      const obj = {
        id: index,
        descripcion: index,
      };
      this.rangoPuntajes.push(obj);
    }
  }

  obtenerPerfiles() {
    this.evaluacionConocimientosService
    .comboPerfiles(this.g.baseId.value)
    .subscribe((res) => {
      this.perfiles = res;
    });
  }

  obtenerEvaluaciones() {
    this.listaGestionNotasRepository
    .getComboEvaluacionesPostulante(this.g.baseId.value)
    .subscribe((res) => {
      this.evaluacion = res;
    });
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      perfil: null,
      periodo: '',
      estado: null,
      grupo: null,
      rangoDesde: null,
      rangoHasta: null,
    });
  }

  limpiar() {
    this.initializeForm();
  }

  buscar() {
    if (!this.showCompleateFilter) {
      this.showCompleateFilter = true;
    }

    this.filtros.perfilId = this.f.perfil.value;
    this.filtros.notaMenor = this.f.rangoDesde.value;
    this.filtros.notaMayor = this.f.rangoHasta.value;
    this.filtros.estadoId = this.f.estado.value;
    this.filtros.fechaInicioPostulacion =
      this.f.periodo.value.start === undefined
        ? ''
        : moment(this.f.periodo.value.start).format('DD/MM/yyyy');
    this.filtros.fechaFinPostulacion =
      this.f.periodo.value.end === undefined
        ? ''
        : moment(this.f.periodo.value.end).format('DD/MM/yyyy');
    this.page = 0;
    this.getData();
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.data);
  }

  private getData() {
    this.evaluacionConocimientosService.buscarSeguimientoEvaluacion(this.datosEtapa.tipoEvaluacion, this.g.baseId.value, this.filtros).subscribe((mc) => {
      this.data = mc;
    });

  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de Contratos';
    model.headers = [
      '#',
      'PERFIL',
      'POSTULANTE',
      'RESULTADO',
      'FECHA POSTULACION',
      'ESTADO',
    ];
    model.keys = [
      'id',
      'nombrePuesto',
      'nombrePostulante',
      'resultado',
      'fechaPostulacion',
      'estado',
    ];
    return model;
  }


  onChangePerfil(val) {

    this.listaGestionNotas.getComboGrupos(this.g.baseId.value ,val).subscribe((mc) => {
      this.grupo = mc;
    });

  }
}
