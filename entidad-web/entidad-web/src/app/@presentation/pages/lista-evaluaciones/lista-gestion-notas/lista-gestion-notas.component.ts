import { MatDialog } from '@angular/material/dialog';
import { Component, ElementRef, OnInit, ViewChild } from '@angular/core';
import { FormGroup, FormBuilder, FormControl } from '@angular/forms';
import { TableColumn } from '../../../@common-components/material-table/table-column';
import {
  CboExamen,
  CboGrupo,
  CboPerfil,
  ICombo,
  IPostulantes,
} from '../../../../@data/model/lista-evaluaciones/entity';
import { sortDataTableComponent } from '../../../../utils/general';
import { Sort } from '@angular/material/sort';
import { Router } from '@angular/router';

import { ListaGestionNotasRepository } from 'src/app/@domain/repository/lista-gestion-notas.repository';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { ExportExcelModel } from 'src/app/@presentation/@service/export-excel.service';
import { Observable, of } from 'rxjs';
import { map } from 'rxjs/operators';
import { DialogRegistroNotasEvalComponent } from './dialogo-registro-notas-eval/dialog-registro-notas-eval.component';
import { DialogoRegistroOtrasNotasComponent } from './dialogo-registro-otras-notas/dialogo-registro-otras-notas.component';
import { ToastService } from '../../../@common-components/toast';

@Component({
  selector: 'serv-talento-lista-gestion-notas',
  templateUrl: './lista-gestion-notas.component.html',
  styleUrls: ['./lista-gestion-notas.component.scss'],
})
export class ListaGestionNotasComponent implements OnInit {
  butonDisappear: boolean = false;
  flagMasivo: boolean = false;
  filtroUp: boolean = true;
  filterForm: FormGroup;
  evaluacionesTableColumns: TableColumn[];
  evaluacionesTableColumnsAll: TableColumn[];
  lstConocimientos: ICombo[];
  lstEvaluaciones: IPostulantes[] = [];
  lstGeneral: any[] = [];
  lstPerfil: CboPerfil[];
  lstGrupo: CboGrupo[];
  lstExamen: CboExamen[];
  lstModalidad: any[];
  convocatoriaId: string;
  resultadoSize: number;
  resultadoSizeAll: number;
  califican: number;
  noCalifican: number;
  noAsistio: number;
  lstEvaluacion: any[];
  flagConocimiento: boolean = false;
  flagCondicion: boolean;
  viewTableOtros: boolean = false;
  viewTableAll: boolean = false;
  modalDescripcion: string;
  codProg: number;
  evaluacionesTableColumnsOtroAll: TableColumn[];
  viewTableOtrosAll: boolean = false;


  filteredOptionsExamen$: Observable<CboExamen[]>;
  @ViewChild('inputExamen') inputExamen: ElementRef;
  optionsExamen: CboExamen[] = [];
  examen: CboExamen[];

  filteredOptionsGrupo$: Observable<CboGrupo[]>;
  @ViewChild('inputGrupo') inputGrupo: ElementRef;
  optionsGrupo: CboGrupo[] = [];
  grupo: CboGrupo[];

  constructor(
    private dialog: MatDialog,
    private listaGestionNotasRepository: ListaGestionNotasRepository,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    private fb: FormBuilder,
    private router: Router,
    private toastService: ToastService
  ) {
    this.filterForm = this.fb.group({
      perfilId: new FormControl(null),
      modalidadId: new FormControl(null),
      nroDocumento: new FormControl(null),
      nombres: new FormControl(null),
      estado: new FormControl(null),
      examenId: new FormControl(null),
      desExamen: new FormControl(null),

      programacionId: new FormControl(null),
      desGrupo: new FormControl(null),
      evaluaciones: new FormControl(null),
    });
  }

  ngOnInit(): void {
    this.convocatoriaId = sessionStorage.getItem('convocatoriaId');
    this.listarComboEvaluaciones();
    this.initializeColumnsAll();
    // this.getDataTableAll();
    this.initializeColumns();
    this.initializeColumnsOtroAlls();
    this.listarComboModalidades();
    this.listarComboEvaluacion();
  }

  getDataTableAll() {
    this.listaGestionNotasRepository
      .getDataTableAll(Number(this.convocatoriaId))
      .subscribe((res) => {
        this.lstGeneral = res.listaDetalleBandeja;

        this.califican = res.califica;
        this.noCalifican = res.noCalifica;
        this.noAsistio = 0;
        this.resultadoSizeAll = res.total;
        this.resultadoSize = res.count;

        this.lstGeneral.forEach(function (part, index, theArray) {
          theArray[index].correlativo = index + 1;
        });
      });

    this.viewTableAll = true;
  }

  buscar(): void {
    if ( this.filterForm.value.evaluaciones == null ) {
      this.toastService.showToast("Seleccione una Evaluación", 'warning');
    } else {

      this.butonDisappear = true;
      let filtro = this.filterForm.value;

      this.listaGestionNotasRepository
        .getListaEvaluacionesPostulante(filtro, this.convocatoriaId)
        .subscribe((res) => {
          this.lstEvaluaciones = res.items  || [];

          this.lstEvaluaciones.forEach((element) => {
            element.toView = element.estadoExamen !== 1;
          });

          this.califican = res.contadorCalifica;
          this.noCalifican = res.contadorNoCalifica;
          this.noAsistio = res.contadorNoAsistio;
          this.resultadoSize = res.count;
          this.lstEvaluaciones.forEach(function (part, index, theArray) {
            theArray[index].correlativo = index + 1;
          });
        });
      this.viewTableAll = false;
      this.viewTableOtros = this.codProg === 1;
      this.viewTableOtrosAll = this.codProg !== 1;
    }
  }

  mapearFormulario(): any {
    let filtro = this.filterForm.value;
    return filtro;
  }
  ocultarFiltro(): void {
    this.filtroUp = !this.filtroUp;
  }
  limpiar(): void {}

  initializeColumns(): void {
    this.evaluacionesTableColumns = [
      {
        name: '#',
        dataKey: 'correlativo',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'NOMBRES Y APELLIDOS',
        dataKey: 'nombre',
        position: 'left',
        isSortable: true,
        width: '23%',
      },
      {
        name: 'DNI',
        dataKey: 'nroDocumento',
        position: 'left',
        isSortable: true,
        width: '8%',
      },
      {
        name: 'PERFIL',
        dataKey: 'desPerfil',
        position: 'left',
        isSortable: true,
        width: '17%',
      },
      {
        name: 'GRUPO',
        dataKey: 'desGrupo',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'EXÁMEN',
        dataKey: 'desExamen',
        position: 'left',
        isSortable: true,
        width: '15%',
      },
      {
        name: 'NOTAS',
        dataKey: 'nota',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'CONDICIÓN',
        dataKey: 'desEstado',
        position: 'left',
        isSortable: true,
        width: '7%',
      },
    ];
  }

  initializeColumnsAll(): void {
    this.evaluacionesTableColumnsAll = [
      {
        name: '#',
        dataKey: 'correlativo',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'NOMBRES Y APELLIDOS',
        dataKey: 'nombreCompleto',
        position: 'left',
        isSortable: true,
        width: '30%',
      },
      {
        name: 'PERFIL',
        dataKey: 'nombrePuesto',
        position: 'left',
        isSortable: true,
        width: '30%',
      },

      {
        name: 'RESULTADO',
        dataKey: 'nota',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'CONDICIÓN',
        dataKey: 'descripcionestadoEvaluacion',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
    ];
  }

  initializeColumnsOtroAlls(): void {
    this.evaluacionesTableColumnsOtroAll = [
      {
        name: '#',
        dataKey: 'correlativo',
        position: 'left',
        isSortable: true,
        width: '7%',
      },
      {
        name: 'NOMBRES Y APELLIDOS',
        dataKey: 'nombre',
        position: 'left',
        isSortable: true,
        width: '30%',
      },
      {
        name: 'DNI',
        dataKey: 'nroDocumento',
        position: 'left',
        isSortable: true,
        width: '15%',
      },
      {
        name: 'PERFIL',
        dataKey: 'desPerfil',
        position: 'left',
        isSortable: true,
        width: '25%',
      },
      {
        name: 'CONDICIÓN',
        dataKey: 'desEstado',
        position: 'left',
        isSortable: true,
        width: '15%',
      },
    ];
  }

  downloadPDF(postulante: any) {
    let cabeceraId = postulante.idCabecera;
    if (cabeceraId) {
      this.gestionPDF(cabeceraId);
    }
  }

  gestionPDF(cabeceraId: number) {
    this.listaGestionNotasRepository.getPDFcv(cabeceraId).subscribe((res) => {
      const base64Data = res;
      const byteArray = new Uint8Array(
        atob(base64Data)
          .split('')
          .map((char) => char.charCodeAt(0))
      );
      let file = new Blob([byteArray], { type: 'application/pdf' });
      const url = window.URL.createObjectURL(file);
      window.open(url);
    });
  }

  listarEvaluacionesPostulante() {
    const filtro = {
      perfilId: null,
      programacionId: null,
      modalidadId: null,
      examenId: null,
      nombres: null,
      nroDocumento: null,
      estado: null,
    };

    this.listaGestionNotasRepository
      .getListaEvaluacionesPostulante(filtro, this.convocatoriaId)
      .subscribe((res) => {
        this.lstEvaluaciones = res;

        this.lstEvaluaciones.forEach((element) => {
          element.toView = element.estadoExamen !== 1;
        });

        this.resultadoSize = this.lstEvaluaciones.length || 0;
        this.lstEvaluaciones.forEach(function (part, index, theArray) {
          theArray[index].correlativo = index + 1;
        });
      });
  }

  listarComboEvaluaciones() {
    this.listaGestionNotasRepository
      .getComboEvaluaciones(this.convocatoriaId)
      .subscribe((res) => {
        this.lstPerfil = res;
      });
  }

  cambioPerfil(idPerfil: number) {
    this.cleanCombos();

    if (idPerfil) {
      this.listaGestionNotasRepository
        .getComboGrupos(this.convocatoriaId, idPerfil)
        .subscribe((res) => {
          this.lstGrupo = res;
          this.filteredOptionsGrupo$ = of(res);
          this.optionsGrupo = this.lstGrupo;
        });

      this.listaGestionNotasRepository
        .getComboExamen(this.convocatoriaId, idPerfil)
        .subscribe((res) => {
          this.lstExamen = res;
          this.filteredOptionsExamen$ = of(res);
          this.optionsExamen = this.lstExamen;
        });
    }
  }

  cleanCombos() {
    this.lstGrupo = [];
    this.lstExamen = [];
    this.filterForm.controls.programacionId.setValue(null);
    this.filterForm.controls.desGrupo.setValue('');

    this.filterForm.controls.examenId.setValue(null);
    this.filterForm.controls.desExamen.setValue('');
  }

  private listarComboModalidades() {
    this.evaluacionConocimientosService
      .listarModalidadEvaluacion()
      .subscribe((res) => {
        this.lstModalidad = res;
      });
  }

  onSelectionChangeModalidad(event: any) {
    if (event === 1) {
      this.flagMasivo = true;
    } else {
      this.flagMasivo = false;
    }
  }

  getDataExportEvaluacionesOtrosAll(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de Evaluaciones';
    model.headers = [
      '#',
      'NOMBRE',
      'N° DOCUMENTO',
      'PERFIL',
      'GRUPO',
      'EXAMEN',
      'NOTA',
      'CONDICION',
    ];
    model.keys = [
      'correlativo',
      'nombre',
      'nroDocumento',
      'desPerfil',
      'desGrupo',
      'desExamen',
      'nota',
      'desEstado',
    ];
    return model;
  }

  getDataExportEvaluacionesAll(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de Resultados';
    model.headers = [
      '#',
      'NOMBRE Y APELLIDOS',
      'PERFIL',
      'RESULTADO',
      'CONDICION',
    ];
    model.keys = ['correlativo', 'nombres', 'desPerfil', 'nota', 'desEstado'];
    return model;
  }
  getDataExportEvaluaciones(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de Evaluaciones';
    model.headers = [
      '#',
      'NOMBRE',
      'N° DOCUMENTO',
      'PERFIL',
      'CONDICION',
    ];
    model.keys = [
      'correlativo',
      'nombre',
      'nroDocumento',
      'desPerfil',
      'desEstado',
    ];
    return model;
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.lstConocimientos);
  }

  viewDetailEvaluacion(postulante: any) {
    localStorage.setItem('datosPostulanteResumen', JSON.stringify(postulante));
    this.router.navigateByUrl(
      '/pages/lista-evaluaciones/resumen-evaluacion-notas'
    );
  }

  private filterExamen(value: string): CboExamen[] {
    const filterValue = value?.toLowerCase();
    return this.optionsExamen?.filter((optionValue) =>
      optionValue.desExamen == null
        ? ''
        : optionValue.desExamen.toLowerCase().includes(filterValue)
    );
  }

  getFilteredOptionsExamen(value: string): Observable<CboExamen[]> {
    return of(value).pipe(
      map((filterString) => this.filterExamen(filterString))
    );
  }

  onChangeExamen() {
    this.filteredOptionsExamen$ = this.getFilteredOptionsExamen(
      this.inputExamen.nativeElement.value
    );

    this.onSelectionChangeExamen(this.inputExamen.nativeElement.value);
  }

  onSelectionChangeExamen($event) {
    this.setExamen($event);
    this.filteredOptionsExamen$ = this.getFilteredOptionsExamen($event);
  }

  setExamen(item: string) {
    let lstExamen = this.lstExamen?.filter((o) => o.desExamen === item)[0]
      ?.idExamen;

    this.filterForm.get('examenId').setValue(lstExamen);
  }

  setExamenById(item: number) {
    let examen = this.lstExamen?.filter((o) => o.idExamen === item)[0]
      ?.idExamen;
    this.filterForm.get('examenId').setValue(examen);
  }

  private filterGrupo(value: string): CboGrupo[] {
    const filterValue = value?.toLowerCase();
    return this.optionsGrupo?.filter((optionValue) =>
      optionValue.desGrupo == null
        ? ''
        : optionValue.desGrupo.toLowerCase().includes(filterValue)
    );
  }

  getFilteredOptionsGrupo(value: string): Observable<CboGrupo[]> {
    return of(value).pipe(
      map((filterString) => this.filterGrupo(filterString))
    );
  }

  onChangeGrupo() {
    this.filteredOptionsGrupo$ = this.getFilteredOptionsGrupo(
      this.inputGrupo.nativeElement.value
    );

    this.onSelectionChangeGrupo(this.inputGrupo.nativeElement.value);
  }

  onSelectionChangeGrupo($event) {
    this.setGrupo($event);
    this.filteredOptionsGrupo$ = this.getFilteredOptionsGrupo($event);
  }

  setGrupo(item: string) {
    let lstGrupo = this.lstGrupo?.filter((o) => o.desGrupo === item)[0]
      ?.idProgramacion;

    this.filterForm.get('programacionId').setValue(lstGrupo);
  }

  setGrupoById(item: number) {
    let grupo = this.lstGrupo?.filter((o) => o.idProgramacion === item)[0]
      ?.idProgramacion;
    this.filterForm.get('programacionId').setValue(grupo);
  }

  subirNotas() {
    let dialogRef = this.dialog.open(DialogRegistroNotasEvalComponent, {
      width: '50.5rem',
      maxHeight: '150vh',
      data: {
        tipo: this.modalDescripcion,
      },
    });

    dialogRef.afterClosed().subscribe(result => {
      this.getDataTableAll();
    });
  }

  subirCondicion() {
    this.dialog.open(DialogoRegistroOtrasNotasComponent, {
      width: '50.5rem',
      maxHeight: '150vh',
      data: {
        tipo: this.modalDescripcion,
        tipoEvaluacion: this.codProg,
      },
    });
  }

  listarComboEvaluacion() {
    let convId = Number(this.convocatoriaId);
    this.listaGestionNotasRepository
      .getComboEvaluacionesPostulante(convId)
      .subscribe((res) => {
        this.lstEvaluacion = res;
      });
  }

  get f() {
    return this.filterForm.controls;
  }

  onChangeEvaluacion($event: any) {
    let codprog = null;
    this.flagMasivo = false;

    this.f.desGrupo.setValue(null);
    this.f.programacionId.setValue(null);
    this.f.modalidadId.setValue(null);
    this.f.desExamen.setValue(null);
    this.f.examenId.setValue(null);

    if ($event) {
      const obj = this.lstEvaluacion.filter(
        (element) => element.evaluacionDetalleId === $event
      );

      codprog = obj[0].codProg;
      this.codProg = +codprog;
      this.modalDescripcion = obj[0].descripcion;
      this.flagConocimiento = codprog === '1';
      this.flagCondicion = codprog === '1'? false : codprog === '5' ? false : true;

      this.viewTableAll = false;
      this.viewTableOtros = codprog === '1';
      this.viewTableOtrosAll = codprog !== '1';
    }
  }
}
