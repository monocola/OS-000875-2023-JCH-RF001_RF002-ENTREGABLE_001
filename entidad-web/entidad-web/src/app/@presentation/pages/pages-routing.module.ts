import { RouterModule, Routes } from '@angular/router';
import { NgModule } from '@angular/core';

import { PagesComponent } from './pages.component';
import { DefaultPageComponent } from './default-page/default-page.component';
import { NotFoundComponent } from './miscellaneous/not-found/not-found.component';

import { AdminEntidadGuard, AdminServirGuard } from '../../@data/guards/index.guard';
import { SidenavService } from 'src/app/@data/services/sidenav.service';
import { GestorOrhGuard } from 'src/app/@data/guards/gestor_orh.guard';
import { MasterGuard } from 'src/app/@data/guards/master.guard';
import { SuperAdminEntidadGuard } from 'src/app/@data/guards/super.admin.entidad.guard';

const routes: Routes = [
  {
    path: '',
    component: PagesComponent,
    children: [
      {
        path: 'home',
        component: DefaultPageComponent,
        data: {
          title: 'Home',
        },
      },
      {
        path: 'configuracion-evaluacion',
        data: {
          title: 'Configuración evaluación - Servir Talento Perú',
        },
        loadChildren: () =>
          import(
            './evaluacion-configuracion/evaluacion-configuracion.module'
          ).then((m) => m.EvaluacionConfiguracionModule),
      },
      {
        path: 'seguimientoconvocatoria',
        data: {
          title: 'Proceso de seguimiento - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./seguimiento-proceso/seguimiento-proceso.module').then(
            (m) => m.SeguimientoProcesoModule
          ),
      },
      {
        path: 'evaluacion-conocimientos',
        data: {
          title: 'Evaluacion de Conocimientos - Servir Talento Perú',
        },
        loadChildren: () =>
          import(
            './evaluacion-conocimientos/evaluacion-conocimientos.module'
          ).then((m) => m.EvaluacionConocimientosModule),
      },
      {
        path: 'lista-evaluaciones',
        data: {
          title: 'Lista de evaluaciones - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./lista-evaluaciones/lista-evaluaciones.module').then(
            (m) => m.ListaEvaluacionesModule
          ),
      },
      {
        path: 'reportes',
        data: {
          title: 'Reportes - Servir Talento Perú',
        },
        loadChildren: () =>
          import(
            './reportes/reportes.module'
          ).then((m) => m.ReportesModule),
      },
      {
        path: 'gestionbases',
        // canActivate: [MasterGuard],
        data: {
          // guards: [AdminEntidadGuard, GestorOrhGuard, CoordinadorGuard],
          title: 'Gestiones de bases - Servir Talento Perú',
          // guardsRelation: 'OR',
        },
        loadChildren: () =>
          import('./bases/bases.module').then((m) => m.BasesModule),
      },
      {
        path: 'gestionsolicitud',
        canActivate: [MasterGuard],
        data: {
          guards: [AdminServirGuard],
          title: 'Gestiones de solicitud - Servir Talento Perú',
        },
        loadChildren: () =>
          import(
            './administrador-solicitudes/administrador-solicitudes.module'
          ).then((m) => m.AdministradorSolicitudesModule),
      },
      {
        path: 'entidad',
        data: {
          // guards: [AdminEntidadGuard],
          title: 'Entidad - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./entidad-perfil/entidad-perfil.module').then(
            (m) => m.EntidadPerfilModule
          ),
      }, 
      {
        path: 'organigrama',
        // canActivate: [MasterGuard],
        data: {
          // guards: [ConfiguradorGuard, AdminEntidadGuard],
          title: 'Gestión de organigrama - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./organigrama/organigrama.module').then(
            (m) => m.OrganigramaModule
          ),
      },
      {
        path: 'test',
        canActivate: [MasterGuard],
        data: {
          guards: [GestorOrhGuard,SuperAdminEntidadGuard ],
          title: '🦎[TEST MODE]🦎 Servir',
        },
        loadChildren: () =>
          import('./test/test.module').then((m) => m.TestModule),
      },
      {
        path: 'cuentasasociadas',
        // canActivate: [MasterGuard],
        data: {
          // guards: [AdminEntidadGuard],
          title: 'Cuentas asociadas - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./cuentas-asociadas/cuentas-asociadas.module').then(
            (m) => m.CuentasAsociadasModule
          ),
      },
      {
        path: 'regperfilpuesto',
        // canActivate: [MasterGuard],
        data: {
          // guards: [AdminEntidadGuard, GestorOrhGuard],
          title: 'Perfiles - Servir Talento Perú',
          // guardsRelation: 'OR',
        },
        loadChildren: () =>
          import('./perfiles/perfiles.module').then((m) => m.PerfilesModule),
      },
      {
        path: 'configsedesentidad',
        // canActivate: [MasterGuard],
        data: {
          // guards: [AdminEntidadGuard],
          title: 'Sedes - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./sedes/sedes.module').then((m) => m.SedesModule),
      },
      {
        path: 'gestionevaluaciones',
        canActivate: [MasterGuard],
        data: {
          guards: [AdminServirGuard],
          title: 'Gestión de evaluaciones - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./evaluacion-servir/evaluacion-servir.module').then(
            (m) => m.EvaluacionServirModule
          ),
      },
      {
        path: 'gestionbonificaciones',
        canActivate: [MasterGuard],
        data: {
          guards: [AdminServirGuard],
          title: 'Gestión de bonificaciones - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./bonificaciones/bonificaciones.module').then(
            (m) => m.BonificacionesModule
          ),
      },
      {
        path: 'gestionmaestras',
        canActivate: [MasterGuard],
        data: {
          guards: [AdminServirGuard],
          title: 'Gestión de tablas maestras - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./maestra-servir/maestra-servir.module').then(
            (m) => m.TablaMaestraModule
          ),
      },
      {
        path: 'gestionimagenes',
        // canActivate: [MasterGuard],
        data: {
          guards: [AdminServirGuard],
          title: 'Gestión de imágenes - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./imagenes/imagenes.modules').then((m) => m.ImagenModule),
      },
      {
        path: 'configmaestrasentidad',
        // canActivate: [MasterGuard],
        data: {
          // guards: [AdminEntidadGuard],
          title: 'Gestión de tablas de convocatoria - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./maestra-orh/maestra-orh.module').then(
            (m) => m.MaestraOrhModule
          ),
      },
      {
        path: 'gestionplantillas',
        // canActivate: [MasterGuard],
        data: {
          // guards: [AdminEntidadGuard],
          title: 'Gestión de plantillas de las bases - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./bases-plantillas/bases-plantillas.module').then(
            (m) => m.BasesPlantillasModule
          ),
      },
      {
        path: 'gestionconocimiento',
        canActivate: [MasterGuard],
        data: {
          guards: [AdminServirGuard],
          title: 'Gestión de conocimientos - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./conocimiento/conocimiento.modules').then(
            (m) => m.ConocimientoModule
          ),
      },
      {
        path: 'gestionevaluaciones-entidad',
        // canActivate: [MasterGuard],
        data: {
          // guards: [AdminEntidadGuard],
          title: 'Gestión de evaluaciones - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./evaluacion-entidad/evaluacion-entidad.module').then(
            (m) => m.EvaluacionEntidadModule
          ),
      },
      {
        path: 'seguimientoconvocatoria',
        data: {
          title: 'Seguimientos - Comunicados - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./seguimiento-comunicado/seguimiento-comunicado.module').then(
            (m) => m.SeguimientoComunicadoModule
          ),
      },
      {
        path: 'seguimiento',
        data: {
          title: 'Proceso de seguimiento - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./seguimiento/seguimiento.module').then(
            (m) => m.SeguimientoModule
          ),
      },
      {
        path: 'seguimiento-eleccion',
        data: {
          title: 'Seguimiento | Elección',
        },
        loadChildren: () =>
          import('./seguimiento-eleccion/seguimiento-eleccion.module').then(
            (m) => m.SeguimientoEleccionModule
          ),
      },
      {
        path: 'seguimiento-evaluacion',
        data: {
          title: 'Seguimiento | Evaluación',
        },
        loadChildren: () =>
          import('./seguimiento-evaluacion/seguimiento-evaluacion.module').then(
            (m) => m.SeguimientoEvaluacionModule
          ),
      },
      {
        path: 'etapas',
        data: {
          title: 'Etapas',
        },
        loadChildren: () =>
          import('./etapas/etapas.module').then((m) => m.EtapasModule),
      },
      {
        path: 'evaluacion-curricular',
        data: {
          title: 'Evaluacion curricular - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./evaluacion-curricular/evaluacion-curricular.module').then(
            (m) => m.EvaluacionCurricularModule
          ),
      },
      {
        path: 'generacioncontrato',
        data: {
          title: 'Gestión de Contratos - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./contratos/contratos.module').then((m) => m.ContratosModule),
      },
      {
        path: 'generacionconvenio',
        data: {
          title: 'Gestión de Convenios - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./convenios/convenios.module').then((m) => m.ConveniosModule),
      },
      {
        path: 'cuadromando',
        data: {
          title: 'Cuadro de Mando - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./cuadro-mando/cuadro-mando.module').then(
            (m) => m.CuadroMandoModule
          ),
      },
      {
        path: 'reporteservir',
        data: {
          title: 'Reporte Servir - Servir Talento Perú',
        },
        loadChildren: () =>
          import('./reporte-servir/reporte-servir.module').then(
            (m) => m.ReporteServirModule
          ),
      },
      {
        path: '',
        redirectTo: 'home',
        pathMatch: 'full',
      },
      {
        path: '**',
        data: {
          title: '404 No encontrado👻 - Servir Talento Perú',
        },
        component: NotFoundComponent,
      },
    ],
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class PagesRoutingModule {
  menu = [];

  constructor(private sidenavService: SidenavService) {}
}
