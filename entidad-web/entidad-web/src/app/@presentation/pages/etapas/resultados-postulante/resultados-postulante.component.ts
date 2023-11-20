import { forkJoin } from 'rxjs';
import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Const } from '../../../../@data/services/const';
import { MatDialog } from '@angular/material/dialog';
import { ToastService } from '../../../@common-components/toast';
import { ModalCalificarSeccionComponent } from '../modal-calificar-seccion/modal-calificar-seccion.component';
import { NbPosition, NbTrigger } from '@nebular/theme';
import { ResultadosPostulanteService } from '../../../../@data/services/resultados-postulante.service';
import { FileVisualizerComponent } from 'src/app/@presentation/@common-components/file-visualizer/file-visualizer.component';

export interface PeriodicElement {
  grado: string;
  situacion: number;
  carrera: string;
}

const ELEMENT_DATA: PeriodicElement[] = [
  {grado: 'Hydrogen', situacion: 1.0079, carrera: 'H'},
  {grado: 'Helium', situacion: 4.0026, carrera: 'He'},
  {grado: 'Lithium', situacion: 6.941, carrera: 'Li'}
];

@Component({
  selector: 'serv-talento-resultados-postulante',
  templateUrl: './resultados-postulante.component.html',
  styleUrls: ['./resultados-postulante.component.scss']
})
export class ResultadosPostulanteComponent implements OnInit {

  popHint: NbTrigger = NbTrigger.HINT;
  pospopRigth: NbPosition = NbPosition.END;

  checkRadioButton = false;

  urlMaestra: string;

  listaFormacionAcademicaSeleccion: any[] = [];
  listaFormacionSuperiorSeleccion: any[] = [];
  listaIdiomasSeleccion: any[] = [];
  listaOfimaticaSeleccion: any[] = [];
  listaEspecializacionSeleccion: any[] = [];
  listaExperienciaSeleccion: any[] = [];
  datosPostulanteSeleccion: any = {};
  datosOtrosRequisitosSeleccion: any = {};

  listaInvestigacionesPostul: any[] = [];

  listaConocimientos: any[] = [];
  listaConocTecnicos: any[] = [];
  listaCursosEspecial: any[] = [];
  listaProgramasEspecial: any[] = [];
  listaConocOfimatica: any[] = [];
  listaConocIdiomas: any[] = [];

  listaReqFormacionSup: any[] = [];

  listaDeclaraJuradaEntidad: any[] = [];
  listaDeclaraJuradaServir: any[] = [];

  dispTrabajarIntPais: any;
  objConvocatoriaPostulante: any;
  objDatosOtrosReq: any;


  constructor(
    public router: Router,
    private dialog: MatDialog,
    private toast: ToastService,
    private resultadosPostulanteService: ResultadosPostulanteService
  ) { }

  ngOnInit(): void {
    this.urlMaestra = Const.API_FILE_SERVER;
    this.objConvocatoriaPostulante = JSON.parse(sessionStorage.getItem("convocatoriaPostulante"));
    this.listarDatos();
    // this.listarDatosFormacionBasicaSeleccion();
    // this.listarDatosFormacionSuperiorSeleccion();
    // this.listarIdiomasSeleccion();
    // this.listarOfimaticaSeleccion();
    // this.listarEspecializacionSeleccion();
    // this.listarExperienciaSeleccion();
    // this.obtenerDatosPostulanteSeleccion();
    // this.obtenerOtrosRequisitosSeleccion();
    // this.obtenerDeclaraJuradaSeleccion();
    // this.obtenerFormacionAcademica();
  }

  listarDatos() {
    const lstFormacBasic = this.resultadosPostulanteService.listarFormacionBasicaSeleccion(this.objConvocatoriaPostulante.convocatoriaPostulanteId);
    const lstFormacSup = this.resultadosPostulanteService.listarFormacionSuperiorSeleccion(this.objConvocatoriaPostulante.convocatoriaPostulanteId);
    const lstIdiomas = this.resultadosPostulanteService.listarIdiomasSeleccion(this.objConvocatoriaPostulante.convocatoriaPostulanteId);
    const lstOfimatica = this.resultadosPostulanteService.listarOfimaticaSeleccion(this.objConvocatoriaPostulante.convocatoriaPostulanteId);
    const lstEspecializacion = this.resultadosPostulanteService.listarEspecializacionSeleccion(this.objConvocatoriaPostulante.convocatoriaPostulanteId);
    const lstExpediencias = this.resultadosPostulanteService.listarExperienciaSeleccion(this.objConvocatoriaPostulante.convocatoriaPostulanteId);
    const datosPostulante = this.resultadosPostulanteService.obtenerDatosPostulanteSeleccion(this.objConvocatoriaPostulante.convocatoriaPostulanteId);
    const datosOtrosReq = this.resultadosPostulanteService.listarOtrosRequisitosSeleccion(this.objConvocatoriaPostulante.convocatoriaPostulanteId);
    const lstDDJJ = this.resultadosPostulanteService.listarDeclaraJuradasSeleccion(this.objConvocatoriaPostulante.baseId, this.objConvocatoriaPostulante.postulanteId);
    const lstFormacAcademic = this.resultadosPostulanteService.listarFormacionAcademica(this.objConvocatoriaPostulante.perfilId);
    forkJoin([lstFormacBasic,lstFormacSup,lstIdiomas, lstOfimatica,lstEspecializacion,
              lstExpediencias,datosPostulante,datosOtrosReq,lstDDJJ,lstFormacAcademic]).subscribe((result) => {
      this.listaFormacionAcademicaSeleccion = result[0].items;
      this.listaFormacionSuperiorSeleccion = result[1].items;
      this.listaIdiomasSeleccion = result[2].items;
      this.listaOfimaticaSeleccion = result[3].items;
      this.listaEspecializacionSeleccion = result[4].items;
      this.listaExperienciaSeleccion = result[5].items;
      this.listaExperienciaSeleccion.forEach(
        (item: any, index: number) => {
          item.remuneracionFijaBruta = item.remuneracionFijaBruta?.toFixed(2);
        }
      );
      this.datosPostulanteSeleccion = result[6];

      this.objDatosOtrosReq = result[7];

      this.listaDeclaraJuradaServir = result[8].ltaDeclaracionServir;
      this.listaDeclaraJuradaEntidad = result[8].ltaDeclaracionNormal;

      this.obtenerFormacionAcademica(result[9]);

    });

  }

  obtenerFormacionAcademica(res): void {

    // this.resultadosPostulanteService.listarFormacionAcademica(this.objConvocatoriaPostulante.perfilId)
    //   .subscribe((res: any) => {
        res.lstFormacionAcademica.forEach(element => {

          let descripcionCarrera = "";
          element.lstCarreraFormaAcademica.forEach((det, index) => {
            if(index === element.lstCarreraFormaAcademica.length - 1 ) {
              descripcionCarrera = det.descripcion;
            } else {
              descripcionCarrera = det.descripcion + ", ";
            }
          });

          const objFormarionReqSup = {
            nombreGrado: element.nombreGrado,
            sitAcademicDesc: element.sitAcademiDesc,
            descripcionCarrera: descripcionCarrera,
          };
          this.listaReqFormacionSup.push(objFormarionReqSup);

        });

        this.listaConocimientos = res.lstConocimientos;

        this.listaConocimientos.forEach(
          (item: any, index: number) => {
            if (Const.COD_CONOCIM_TECNICO === item.tipoConocimientoId) {
              this.listaConocTecnicos.push(item);
            }
            if (Const.COD_CURSOS_ESPECIAL === item.tipoConocimientoId) {
              this.listaCursosEspecial.push(item);
            }
            if (Const.COD_PROGRAM_ESPECIAL === item.tipoConocimientoId) {
              this.listaProgramasEspecial.push(item);
            }
            if (Const.COD_CONOCIM_OFIMATICA === item.tipoConocimientoId) {
              this.listaConocOfimatica.push(item);
            }
            if (Const.COD_CONOCIM_IDIOMA === item.tipoConocimientoId) {
              this.listaConocIdiomas.push(item);
            }
          }
        );

      // });
  }

  calificarSeccion(e: any) {
    const modalCalificarSecccion = this.dialog.open(ModalCalificarSeccionComponent, {
      width: '40rem',
      data: {

      },
    });
    modalCalificarSecccion.afterClosed().subscribe((res) => {
      if (res) {
        this.toast.showToast('Registro exitoso', 'success', 'Atención');
      } else {
        let redereci = e.redereci ? false : true;
        e.redereci = redereci;
      }
    });
  }

  openCvPDF() {
    this.resultadosPostulanteService
    .descargarCV(this.objConvocatoriaPostulante.convocatoriaPostulanteId)
    .subscribe((res) => {
       this.dialog.open(FileVisualizerComponent, {
         data: {
           base64String: res,
           filename: 'postulanteCV',
           extension: 'pdf',
         },
       });
    });
  }

  back() {
    sessionStorage.removeItem("convocatoriaPostulante");
    this.router.navigateByUrl('pages/etapas');
  }

}
