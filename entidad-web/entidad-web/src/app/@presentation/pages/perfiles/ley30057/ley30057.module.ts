import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { Ley30057RoutingModule } from './ley30057-routing.module';
import { Ley30057Component } from './ley30057.component';
import { AngularResizedEventModule } from 'angular-resize-event';
import { FuncionesComponent } from './funciones/funciones.component';
import { FormacionComponent } from './formacion/formacion.component';
import { ExperienciaComponent } from './experiencia/experiencia.component';
import { IdentificacionComponent } from './identificacion/identificacion.component';
import { MatStepperModule } from '@angular/material/stepper';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { NgxTrimDirectiveModule } from 'ngx-trim-directive';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatRadioModule } from '@angular/material/radio';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatListModule } from '@angular/material/list';
import { ComponentsPerfilesModule } from '../components/components.perfiles.module';
import { CommonComponentsModule } from 'src/app/@presentation/@common-components/common-components.module';
import {
  NbAutocompleteModule,
  NbButtonModule, NbCardModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbPopoverModule,
  NbSelectModule
} from '@nebular/theme';
import { AutocompleateagrupadoComponent } from './formacion/autocompleateagrupado/autocompleateagrupado.component';

@NgModule({
  declarations: [
    Ley30057Component,
    FuncionesComponent,
    FormacionComponent,
    ExperienciaComponent,
    IdentificacionComponent,
    AutocompleateagrupadoComponent,
  ],
  imports: [
    CommonModule,
    Ley30057RoutingModule,
    CommonComponentsModule,
    NbIconModule,
    NbPopoverModule,
    AngularResizedEventModule,
    MatStepperModule,
    NbButtonModule,
    MatFormFieldModule,
    MatInputModule,
    MatIconModule,
    NbInputModule,
    NbFormFieldModule,
    NgxTrimDirectiveModule,
    NbSelectModule,
    ReactiveFormsModule,
    FormsModule,
    MatRadioModule,
    MatCheckboxModule,
    MatListModule,
    ComponentsPerfilesModule,
    NbAutocompleteModule,
    NbCardModule
  ],
  exports: [ExperienciaComponent, FormacionComponent],
})
export class Ley30057Module {}
