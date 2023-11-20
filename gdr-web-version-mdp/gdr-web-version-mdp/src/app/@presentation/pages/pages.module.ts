import { NgModule } from '@angular/core';
import { ThemeModule } from '../@theme/theme.module';
import { PagesRoutingModule } from './pages-routing.module';
import { PagesComponentsModule } from './components/components.module';
import {
  NbActionsModule,
  NbButtonModule,
  NbCardModule,
  NbLayoutModule,
  NbSelectModule,
  NbSidebarModule,
  NbIconModule,
  NbAutocompleteModule, NbInputModule, NbFormFieldModule
} from '@nebular/theme';

// Components
import { PagesComponent } from './pages.component';
import { DefaultPageComponent } from './default-page/default-page.component';
import { NotBuildedComponent } from './miscellaneous/not-builded/not-builded.component';
import { MatDialogModule } from '@angular/material/dialog';
import { CanDeactivateGuard } from 'src/app/@data/guards/can-deactivate.guard';
import { MasterGuard } from 'src/app/@data/guards/master.guard';
import { ReactiveFormsModule } from '@angular/forms';
import { MatIconModule } from '@angular/material/icon';
import { MatTabsModule } from '@angular/material/tabs';
import { EvaluadosComponent } from './evaluados/evaluados.component';
import { FactoresEvaluacionComponent } from './factores-evaluacion/factores-evaluacion.component';
import { ImplementacionComponent } from './implementacion/implementacion.component';
import { CommonComponentsModule } from '../@common-components/common-components.module';
import { IvyCarouselModule } from 'angular-responsive-carousel';
import { AsignarGestorGdrComponent } from './asignar-entidad/asignar-gestor-gdr.component';
 
@NgModule({
  imports: [
    PagesRoutingModule,
    ThemeModule,
    NbLayoutModule,
    NbSidebarModule,
    PagesComponentsModule,
    MatDialogModule,
    ReactiveFormsModule,
    NbCardModule,
    NbActionsModule,
    NbSelectModule,
    NbButtonModule,
    MatIconModule,
    NbIconModule,
    MatTabsModule,
    NbAutocompleteModule,
    NbInputModule,
    CommonComponentsModule,
    NbFormFieldModule,
    IvyCarouselModule
  ],
  declarations: [PagesComponent, DefaultPageComponent, NotBuildedComponent, EvaluadosComponent, FactoresEvaluacionComponent, ImplementacionComponent, AsignarGestorGdrComponent],
  providers: [CanDeactivateGuard, MasterGuard],
})
export class PagesModule {}
