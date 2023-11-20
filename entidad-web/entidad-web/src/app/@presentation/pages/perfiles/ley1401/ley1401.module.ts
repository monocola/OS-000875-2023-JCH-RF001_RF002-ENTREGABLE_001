import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { Ley1401RoutingModule } from './ley1401-routing.module';
import { Ley1401Component } from './ley1401.component';
import { MatStepperModule } from '@angular/material/stepper';
import { ComponentsPerfilesModule } from '../components/components.perfiles.module';
import { IdentificacionComponent } from './identificacion/identificacion.component';
import {
  NbButtonModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbPopoverModule,
  NbSelectModule,
  NbAutocompleteModule
} from '@nebular/theme';
import { AngularResizedEventModule } from 'angular-resize-event';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { NgxTrimDirectiveModule } from 'ngx-trim-directive';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatListModule } from '@angular/material/list';
import { MatRadioModule } from '@angular/material/radio';
import { FuncionesComponent } from './funciones/funciones.component';
import { FormacionComponent } from './formacion/formacion.component';
import { ExperienciaComponent } from './experiencia/experiencia.component';
import { CommonComponentsModule } from 'src/app/@presentation/@common-components/common-components.module';

@NgModule({
  declarations: [
    Ley1401Component,
    IdentificacionComponent,
    FuncionesComponent,
    FormacionComponent,
    ExperienciaComponent,
  ],
  imports: [
    CommonModule,
    Ley1401RoutingModule,
    MatStepperModule,
    ComponentsPerfilesModule,
    CommonModule,
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
    MatButtonModule
  ],
})
export class Ley1401Module {}
