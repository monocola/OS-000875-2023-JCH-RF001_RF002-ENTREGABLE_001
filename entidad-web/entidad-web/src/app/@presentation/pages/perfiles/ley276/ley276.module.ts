import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { Ley276RoutingModule } from './ley276-routing.module';
import { Ley276Component } from './ley276.component';
import { IdentificacionComponent } from './identificacion/identificacion.component';

import { MatStepperModule } from '@angular/material/stepper';
import { ComponentsPerfilesModule } from '../components/components.perfiles.module';
import { AngularResizedEventModule } from 'angular-resize-event';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatIconModule } from '@angular/material/icon';
import { NgxTrimDirectiveModule } from 'ngx-trim-directive';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatListModule } from '@angular/material/list';
import { MatRadioModule } from '@angular/material/radio';
import {
  NbButtonModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbPopoverModule,
  NbSelectModule,
} from '@nebular/theme';
import { FuncionesComponent } from './funciones/funciones.component';
import { Ley30057Module } from '../ley30057/ley30057.module';
import { CommonComponentsModule } from 'src/app/@presentation/@common-components/common-components.module';

@NgModule({
  declarations: [Ley276Component, IdentificacionComponent, FuncionesComponent],
  imports: [
    CommonModule,
    Ley276RoutingModule,
    NgxTrimDirectiveModule,
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
    Ley30057Module,
  ],
})
export class Ley276Module {}
