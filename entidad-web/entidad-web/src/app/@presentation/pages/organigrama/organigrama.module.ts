import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { OrganigramaRoutingModule } from './organigrama-routing.module';
import { OrganigramaComponent } from './organigrama.component';
import { MatDividerModule } from '@angular/material/divider';
import {
  NbAutocompleteModule,
  NbButtonModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbLayoutModule,
  NbPopoverModule,
  NbSelectModule,
  NbTreeGridModule,
} from '@nebular/theme';
import { ConfiguracionComponent } from './configuracion/configuracion.component';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

import { NgxTrimDirectiveModule } from 'ngx-trim-directive';
import { MatTabsModule } from '@angular/material/tabs';
import { OrganoComponent } from './configuracion/organo/organo.component';
import { UnidadOrganicaComponent } from './configuracion/unidad-organica/unidad-organica.component';
import { RegistroOrganoComponent } from './configuracion/organo/registro-organo/registro-organo.component';
import { RegistroMasivoOrganoComponent } from './configuracion/organo/registro-masivo-organo/registro-masivo-organo.component';
import { RegistroUnidadOrganicaComponent } from './configuracion/unidad-organica/registro-unidad-organica/registro-unidad-organica.component';
import { RegistroMasivoUnidadOrganicaComponent } from './configuracion/unidad-organica/registro-masivo-unidad-organica/registro-masivo-unidad-organica.component';
import { MatButtonModule } from '@angular/material/button';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatChipsModule } from '@angular/material/chips';
import { MatIconModule } from '@angular/material/icon';
import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { VistaComponent } from './vista/vista.component';
import { GraphComponent } from './vista/graph/graph.component';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { NgxSpinnerModule } from 'ngx-spinner';
import { MatListModule } from '@angular/material/list';
import { ThemeModule } from '../../@theme/theme.module';
import { CommonComponentsModule } from '../../@common-components/common-components.module';

@NgModule({
  declarations: [
    OrganigramaComponent,
    ConfiguracionComponent,
    OrganoComponent,
    UnidadOrganicaComponent,
    RegistroOrganoComponent,
    RegistroMasivoOrganoComponent,
    RegistroUnidadOrganicaComponent,
    RegistroMasivoUnidadOrganicaComponent,
    VistaComponent,
    GraphComponent,
  ],
  imports: [
    CommonModule,
    OrganigramaRoutingModule,
    ThemeModule,
    NbButtonModule,
    FormsModule,
    ReactiveFormsModule,
    NbIconModule,
    CommonComponentsModule,
    NbLayoutModule,
    MatDividerModule,
    NbSelectModule,
    NbInputModule,
    ReactiveFormsModule,
    NbFormFieldModule,
    NbAutocompleteModule,
    NgxTrimDirectiveModule,
    MatTabsModule,
    NbPopoverModule,
    MatButtonModule,
    MatPaginatorModule,
    NbTreeGridModule,
    MatChipsModule,
    MatIconModule,
    MatAutocompleteModule,
    MatProgressBarModule,
    NgxSpinnerModule,
    MatListModule,
  ],
})
export class OrganigramaModule {}
