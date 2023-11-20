import { ToastService } from './../../../@common-components/toast';
import { TypeDocument } from './../../../../@data/model/typeDocument';
import { ComboitemModel } from './../../../../@data/model/generic/comboitem.model';
import { EvaluacionCurricularRepository } from 'src/app/@domain/repository/evaluacion-curricular.repository';
import { ReporteEvaluacionesRepository } from 'src/app/@domain/repository/reporte-evaluaciones.repository';
import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormControl, FormGroup, Validators } from '@angular/forms';
import { Ubigeo } from '../../../../@data/model/ubigeo';
import { AuthenticationRepository } from '../../../../@domain/repository/authentication.repository';
import { ReportesRepository } from '../../../../@domain/repository/reportes.repository';
import { ParameterRepository } from '../../../../@domain/repository/parameter.repository';
import { MaestraRepository } from '../../../../@domain/repository/maestra.reposity';
import { Observable, of } from 'rxjs';
import { map, startWith, filter } from 'rxjs/operators';
import { TableColumn } from '../../../@common-components/material-table/table-column';
import { ExportExcelModel } from '../../../@service/export-excel.service';
import { Sort } from '@angular/material/sort';
import { sortDataTableComponent } from '../../../../utils/general';
import { Router } from '@angular/router';
import moment from 'moment';

@Component({
  selector: 'serv-talento-evaluaciones',
  templateUrl: './evaluaciones.component.html',
  styleUrls: ['./evaluaciones.component.scss']
})
export class EvaluacionesComponent implements OnInit {

  filterForm: FormGroup;
  rangePickerStatus: string = 'basic';
  lstRegimen: any[];
  lstDepartamentos: Ubigeo[];
  lstModalidadAcceso: any[];
  entidadId: number;


  options: string[];
  options2: string[];

  columns: TableColumn[];
  data: any[] = [];
  dataHeader: any;
  page: number = 0;
  size: number = 20;

  lstComboEvaluaciones: any[] = [];
  lstComboTipoDocumentos: any[] = [];
  lstComboNombresApellidos: any[] = [];
  lstComboEstadosEvaluacionC: any[] = [];
  lstComboPerfiles: any[] = [];

  convocatoriaId: number = 0; // 98


  convocatorias_autocomplete$: any[] = [];
  _convocatorias_autocomplete: any[] = [];
  lstConvocatoriasId: any;

  postulantes_autocomplete$: any[] = [];
  _postulantes_autocomplete: any[] = [];

  constructor(
    private authenticationRepository: AuthenticationRepository,
    private srvReportesRepository: ReportesRepository,
    private srvParameterRepository: ParameterRepository,
    private fb: FormBuilder,
    public router: Router,
    private reporteEvaluacionesRepository: ReporteEvaluacionesRepository,
    private srvEvaluacionCurricular: EvaluacionCurricularRepository,
    private toastService: ToastService,
  ) {
    this.filterForm = this.fb.group({
      evaluaciones: new FormControl(null),
      convocatoria: new FormControl(null),
      perfiles: new FormControl(null),
      postulantes: new FormControl(null),
      tipoDocumento: new FormControl(null),
      numeroDocumento: new FormControl(null),
      periodo: new FormControl(null),
      redereci: new FormControl(null),
      redam: new FormControl(null),
      rnssc: new FormControl(null),
      estado: new FormControl(null)
    });
  }

  ngOnInit(): void {
    this.entidadId = this.authenticationRepository.getCurrentUserValue.entidadId;
    this.getRegimen();
    this.getDepartamentos();
    this.initializeColumns();
    this.getData();

  }


  initializeColumns() {
    this.columns = [
      {
        name: '#',
        dataKey: 'id',
        position: 'left',
        isSortable: true,
        width: '5%',
      },

    ];
  }


  onInputChangeAutocomplete(event: any, key: string) {
    const search_text = this.filterForm.controls[key].value;

    this.convocatorias_autocomplete$ = this._convocatorias_autocomplete;

    if (key === 'convocatoria') {
       this.convocatorias_autocomplete$ = this._convocatorias_autocomplete.filter(
         (item: any) => {
           if (item.codigoConvocatoria !== null) {
             return (
               item.codigoConvocatoria
                 .toLowerCase()
                 .indexOf(search_text.toLowerCase()) > -1
             );
           }
           return false;
         }
       );
    } else if (key === 'postulantes') {
      this.postulantes_autocomplete$ = this._postulantes_autocomplete.filter(
        (item: any) => {
          if (item.nombreCompleto !== null) {
            return (
              item.nombreCompleto
                .toLowerCase()
                .indexOf(search_text.toLowerCase()) > -1
            );
          }
          return false;
        }
      );
    }
  }


  getRegimen() {
    this.srvReportesRepository.getRegimen(this.entidadId).subscribe((mc) => {
      this.lstRegimen = mc;
    });
  }

  getDepartamentos() {
    this.srvParameterRepository.getDepartamento().subscribe((mc) => {
      this.lstDepartamentos = mc;
    });
  }


  onChangeEvaluacion($event: any) {}

  limpiar(): void {
    this.convocatoriaId = 0;
    this.filterForm.reset();
    this.filterForm.controls['convocatoria'].setValue("");
    this.lstComboEstadosEvaluacionC = [];
    this.lstComboEvaluaciones = [];
    this.getData();
  }

  buscar(): void {

   // if (this.validarRequeridos()) {
  if (true) {


      let periodo = this.filterForm.controls['periodo'].value;
      let periodoIni: string = "Invalid date";
      let periodoFin: string = "Invalid date";
      if (periodo != null) {
        periodoIni = periodo.start;
        periodoFin = periodo.end;
      }



      let payload = {
        evaluacionId: this.filterForm.controls['evaluaciones'].value,
//        codigoConvocatoria: this.filterForm.controls['convocatoria'].value,
        convocatoriaId: this.convocatoriaId,
        nombresApellidos: this.filterForm.controls['postulantes'].value,
        tipoDocumentoId: this.filterForm.controls['tipoDocumento'].value,
        nroDocumento: this.filterForm.controls['numeroDocumento'].value,
        periodoInicio: periodoIni === 'Invalid date' ? null : moment(periodoIni).format('DD-MM-yyyy'),
        periodoFin: periodoFin === 'Invalid date' ? null : moment(periodoFin).format('DD-MM-yyyy'),
        identRedereci: this.filterForm.controls['redereci'].value,
        identRedam: this.filterForm.controls['redam'].value,
        identRnssc: this.filterForm.controls['rnssc'].value,
        estadoId: this.filterForm.controls['estado'].value,
        entidadId: this.entidadId,
        perfilId: this.filterForm.controls['perfiles'].value,
        page: 0,
        size: 20
      };
      console.log(payload);

      this.reporteEvaluacionesRepository.buscarReporteEvaluaciones(payload).subscribe((mc) => {
        this.dataHeader = mc;
        this.data = mc.listRepDetalleEvaluacion;

        this.data.forEach((element, index) => {
          element.id = index+1;
        });
        console.log("Resultado búsqueda");
        console.log(this.data);
      });
    }

  }

  validarRequeridos(): boolean {
    if (this.filterForm.controls['convocatoria'].value === null) {
      this.toastService.showToast('Ingrese un código de convocatoria para continuar', 'info');
      return false;
    }

    if (this.filterForm.controls['periodo'].value === null ||
        this.filterForm.controls['periodo'].value === '') {
      this.toastService.showToast('Ingrese un periodo para continuar', 'info');
      return false;
    }

    if (this.filterForm.controls['evaluaciones'].value === null ) {
      this.toastService.showToast('Ingrese un tipo de evaluación para continuar', 'info');
      return false;

      }

    return true;

  }

  // Funciones para el autocomplete
  validRangeDateFormat(event: any) {
    this.rangePickerStatus = 'basic';
    const fecha = this.filterForm.controls['periodo'].value;

    if (this.filterForm.controls['periodo'].errors && fecha === null) {
      this.rangePickerStatus = 'danger';
    }
  }

  onChangePerfiles($event) {

    this.reporteEvaluacionesRepository.getListaNombresPorConvocatoriaPerfil(this.convocatoriaId, $event).toPromise().then((res: any)=> {
      this._postulantes_autocomplete = res.items;
      this.postulantes_autocomplete$ = res.items;
     });
  }


  onConvocatoriaACSelect($event) {
    let result = this.lstConvocatoriasId.filter((item: any) => {
      if (item.codigoConvocatoria === $event) {
          return item.idConvocatoria;
      } else {
        return null;
      }
    });

    let objConvocatoria = result[0];
    this.convocatoriaId = objConvocatoria.idConvocatoria;
    this.srvReportesRepository.getPerfilesEntidad(this.convocatoriaId).subscribe((mc) => {
      this.lstComboPerfiles = mc;
      console.log(this.lstComboPerfiles);
    });

    this.reporteEvaluacionesRepository.getListaEvaluaciones(objConvocatoria.idConvocatoria,this.entidadId).subscribe((mc) => {
      this.lstComboEvaluaciones = mc.items;
      console.log(this.lstComboEvaluaciones);
    });

  }

  viewHandle(value: string) {
    return value.toUpperCase();
  }

  private getData() {

    this.dataHeader = {
      periodoInicio: "",
      periodoFin: "",
      depName: null,
      depId: null,
      cantConv: 0,
      cantPostulante: 0,
      califican: 0,
      noCalifican: 0,
    };


    this.srvEvaluacionCurricular.listarEstadoEvaluacionCurricular("ESTADO_TODOS").subscribe((mc) => {
      this.lstComboEstadosEvaluacionC = mc;
    });

    this.srvReportesRepository.getCodigoConvocatoria(this.entidadId).toPromise().then((res: any) => {
      this._convocatorias_autocomplete = res;
      this.convocatorias_autocomplete$ = res;
      this.lstConvocatoriasId = res;
    });


    this.srvParameterRepository.getTypeDocuments().toPromise().then((res: any)=> {
      this.lstComboTipoDocumentos = res;
    });

    this.srvReportesRepository.getPerfilesEntidad(this.convocatoriaId).subscribe((mc) => {
      this.lstComboPerfiles = mc;
    });

  }


 openDetalleEvalPostulante(row: any) {
    sessionStorage.setItem('rowEvaluacion', JSON.stringify(row));
    this.router.navigateByUrl('pages/reportes/detalle-evaluaciones');
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.data);
  }

  getPaginado(e) {
    this.page = e.pageIndex;
    this.size = e.pageSize;
    this.getData();
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de evaluaciones';
    model.headers = [
      '#',
      'EVALUACION',
      'CODIGO',
      'REGIMEN',
      'NOMBRES',
      'APELLIDOS',
      'RNSSC',
      'REDERECI',
      'REDAM',
      'NOTA',
      'RESULTADO',
      'ESTADO'
    ];
    model.keys = [
      'id',
      'evaluacion',
      'codigoConvocatoria',
      'regimen',
      'nombres',
      'apellidos',
      'flagRnssc',
      'flagRedereci',
      'flagRedam',
      'nota',
      'resultado',
      'estado'

    ];
    return model;
  }

  verMensaje(e: any) {
    console.log(e);
  }

}

