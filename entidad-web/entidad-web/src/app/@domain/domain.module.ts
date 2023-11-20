import { ReporteEvaluacionesService } from './../@data/services/reporte-evaluaciones.service';
import { ReporteDetalleConvocatoriaService } from './../@data/services/reporte-detalle-convocatoria.service';
import { ConfiguracionReqMinService } from './../@data/services/configuracion-req-min.service';
import { ConfiguracionReqMinRepository } from 'src/app/@domain/repository/configuracion-req-min.repository';
import {
  ModuleWithProviders,
  NgModule,
  Optional,
  SkipSelf,
} from '@angular/core';
import { CommonModule } from '@angular/common';
import {
  AdministratorService,
  AuthenticationService,
  ParameterService,
  SunatService,
} from '../@data/services';
import { throwIfAlreadyLoaded } from './module-import-guard';
import { AuthenticationRepository } from './repository/authentication.repository';
import { ParameterRepository } from './repository/parameter.repository';
import { SunatRepository } from './repository/sunat.repository';
import { AdministratorRepository } from './repository/administrator.repository';

import { ReniecRepository } from './repository/reniec.repository';
import { ReniecService } from '../@data/services/reniec.service';

import { EntidadRepository } from './repository/entidad.repository';
import { EntidadService } from '../@data/services/entidad.service';

import { CuentaEntidadService } from '../@data/services/cuenta.entidad.service';
import { CuentaEntidadRepository } from './repository/cuenta.entidad.repository';

import { OrganoRepository } from './repository/organo.repository';
import { OrganoService } from 'src/app/@data/services/organo.service';

import { UnidadOrganicaService } from 'src/app/@data/services/unidad-organica.service';
import { UnidadOrganicaRepository } from './repository/unidad-organica.repository';

import { OrganigramaRepository } from './repository/organigrama.repository';
import { OrganigramaService } from 'src/app/@data/services/organigrama.service';

import { SedesService } from 'src/app/@data/services/sedes.service';
import { SedesRepository } from './repository/sede.repository';

import { MaestraRepository } from './repository/maestra.reposity';
import { MaestraService } from 'src/app/@data/services/maestra.service';

import { MaestraEntidadRepository } from './repository/maestra-entidad.repository';
import { MaestraEntidadService } from 'src/app/@data/services/maestra-entidad.service';

import { EvaluacionesServirRepository } from './repository/evaluaciones-servir.repository';
import { EvaluacionesServirService } from 'src/app/@data/services/evaluaciones-servir.service';

import { ListaContratoRepository } from './repository/lista-contrato.repository';
import { ListaContratoService } from '../@data/services/lista-contrato.service';

import { PerfilesRepository } from './repository/perfiles.repository';
import { PerfilesService } from 'src/app/@data/services/perfiles.service';

import { BasesPlantillasRepository } from './repository/bases-plantillas.repository';
import { BasesPlantillasService } from '../@data/services/bases.plantillas.service';

import { EvaluacionesEntidadService } from '../@data/services/evaluaciones-entidad.service';
import { EvaluacionesEntidadRepository } from './repository/evaluaciones-entidad.repository';

import { BasesRepository } from './repository/bases.repository';
import { BasesService } from '../@data/services/bases.service';

import { BonificacionesRepository } from './repository/bonificaciones.repository';
import { BonificacionesService } from '../@data/services/bonificaciones.service';

import { EvaluacionConocimientosRepository } from './repository/evaluacion-conocimientos.repository';
import { EvaluacionConocimientosService } from '../@data/services/evaluacion-conocimientos.service';

import { SeguimientoCronogramaRepository } from './repository/seguimiento-cronograma.repository';
import { SeguimientoCronogramaService } from '../@data/services/seguimiento-cronograma.service';
import { EvaluacionCurricularRepository } from './repository/evaluacion-curricular.repository';
import { EvaluacionCurricularService } from '../@data/services/evaluacion-curricular.service';

import { BannerRepository } from './repository/banner.repository';
import { BannerService } from '../@data/services/banner.service';
import { ConocimientoRepository } from './repository/conocimiento.repository';
import { ConocimientoService } from '../@data/services/conocimiento.service';
import { SeguimientoRepository } from './repository/seguimiento.repository';
import { SeguimientoService } from '../@data/services/seguimiento.service';

import { SeleccionServirRepository } from './repository/seleccion-servir-repository';
import { SeleccionServirService } from '../@data/services/seleccion-servir.service';
import { ListaGestionNotasRepository } from './repository/lista-gestion-notas.repository';
import { ListaGestionNotasService } from '../@data/services/lista-gestion-notas.service';

import { ListaGestionOtrasNotasRepository } from './repository/lista-gestion-otras-notas.repository';
import { ListaGestionOtrasNotasService } from '../@data/services/lista-gestion-otras-notas.service';
import { DesestimientoRepository } from './repository/desestimiento.repository';
import { DesestimientoContratoService } from '../@data/services/desestimiento-contrato.service';
import { ResultadosPostulanteRepository } from './repository/resultados-postulante.repository';
import { ResultadosPostulanteService } from '../@data/services/resultados-postulante.service';
import { ReportesRepository } from './repository/reportes.repository';
import { ReportesService } from '../@data/services/reportes.service';
import { ReporteEvaluacionesRepository } from './repository/reporte-evaluaciones.repository';
import { ReporteDetalleConvocatoriaRepository } from './repository/reporte-detalle-convocatoria.repository';
import { GraficosRepository } from './repository/graficos.repository';
import { GraficosService } from '../@data/services/graficos.service';

import { ReportePostulanteRepository } from './repository/reporte-postulante-repository';
import { ReportePostulanteService } from './../@data/services/reporte-postulante-service';
const DATA_SERVICES = [
  {
    provide: AuthenticationRepository,
    useClass: AuthenticationService,
  },
  { provide: ParameterRepository, useClass: ParameterService },
  { provide: SunatRepository, useClass: SunatService },
  { provide: EntidadRepository, useClass: EntidadService },
  {
    provide: CuentaEntidadRepository,
    useClass: CuentaEntidadService,
  },
  {
    provide: AdministratorRepository,
    useClass: AdministratorService,
  },
  { provide: ReniecRepository, useClass: ReniecService },
  { provide: OrganoRepository, useClass: OrganoService },
  {
    provide: UnidadOrganicaRepository,
    useClass: UnidadOrganicaService,
  },
  { provide: SedesRepository, useClass: SedesService },
  { provide: OrganigramaRepository, useClass: OrganigramaService },
  { provide: MaestraRepository, useClass: MaestraService },
  {
    provide: MaestraEntidadRepository,
    useClass: MaestraEntidadService,
  },
  {
    provide: EvaluacionesServirRepository,
    useClass: EvaluacionesServirService,
  },
  {
    provide: ListaContratoRepository,
    useClass: ListaContratoService,
  },
  {
    provide: SeguimientoCronogramaRepository,
    useClass: SeguimientoCronogramaService,
  },
  {
    provide: EvaluacionesEntidadRepository,
    useClass: EvaluacionesEntidadService,
  },
  {
    provide: BasesRepository,
    useClass: BasesService,
  },
  { provide: PerfilesRepository, useClass: PerfilesService },
  { provide: BasesPlantillasRepository, useClass: BasesPlantillasService },
  { provide: BonificacionesRepository, useClass: BonificacionesService },
  {
    provide: EvaluacionConocimientosRepository,
    useClass: EvaluacionConocimientosService,
  },
  {
    provide: EvaluacionCurricularRepository,
    useClass: EvaluacionCurricularService,
  },
  {
    provide: BannerRepository,
    useClass: BannerService,
  },
  {
    provide: ConocimientoRepository,
    useClass: ConocimientoService,
  },
  {
    provide: SeleccionServirRepository,
    useClass: SeleccionServirService,
  },
  {
    provide: ListaGestionNotasRepository,
    useClass: ListaGestionNotasService,
  },
  {
    provide: ListaGestionOtrasNotasRepository,
    useClass: ListaGestionOtrasNotasService,
  },

  {
    provide: SeguimientoRepository,
    useClass: SeguimientoService,
  },
  {
    provide: ConfiguracionReqMinRepository,
    useClass: ConfiguracionReqMinService,
  },
  {
    provide: DesestimientoRepository,
    useClass: DesestimientoContratoService,
  },
  {
    provide: ResultadosPostulanteRepository,
    useClass: ResultadosPostulanteService,
  },
  {
    provide: ReportesRepository,
    useClass: ReportesService,
  },
  {
    provide: ReporteDetalleConvocatoriaRepository,
    useClass: ReporteDetalleConvocatoriaService,
  },
  {
    provide: ReporteEvaluacionesRepository,
    useClass: ReporteEvaluacionesService,
  },
  {
    provide: GraficosRepository,
    useClass: GraficosService,
  },
  {
    provide: ReportePostulanteRepository,
    useClass: ReportePostulanteService,
  }
];

@NgModule({
  declarations: [],
  imports: [CommonModule],
})
export class DomainModule {
  constructor(@Optional() @SkipSelf() parentModule: DomainModule) {
    throwIfAlreadyLoaded(parentModule, 'CoreModule');
  }

  static forRoot(): ModuleWithProviders<any> {
    return {
      ngModule: DomainModule,
      providers: [...DATA_SERVICES],
    } as ModuleWithProviders<any>;
  }
}
