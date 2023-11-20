import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatDividerModule } from '@angular/material/divider';
import { EntidadComponent } from './entidad.component';
import { EntidadRoutingModule } from './entidad-routing.module';
import {
  NbButtonModule,
  NbSelectModule,
  NbFormFieldModule,
  NbInputModule,
  NbRadioModule,
  NbIconModule,
  NbCardModule
} from '@nebular/theme';
import { MatTabsModule } from '@angular/material/tabs';
import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { ModalEditarComponent } from './modal-editar/modal-editar.component';
import {IvyCarouselModule} from 'angular-responsive-carousel';
import { NgApexchartsModule } from 'ng-apexcharts';
import { GmeComponent } from './gme/gme.component';
import { GdrComponent } from './gdr/gdr.component';
import { DatosEntidadComponent } from './datos-entidad/datos-entidad.component';
import { NgxDropzoneModule } from 'ngx-dropzone';
 
@NgModule({
  declarations: [
    EntidadComponent,
    ModalEditarComponent,
    GmeComponent,
    GdrComponent,
    DatosEntidadComponent
  ],
  imports: [
    EntidadRoutingModule,
    CommonModule,
    NgxDropzoneModule,
    MatDividerModule,
    NbButtonModule,
    NbSelectModule,
    MatTabsModule,
    MatCardModule,
    MatIconModule,
    FormsModule,
    ReactiveFormsModule,
    NbFormFieldModule,
    NbInputModule,
    CommonComponentsModule,
    NbRadioModule,
    NbIconModule,
    NbCardModule,
    IvyCarouselModule,
    FormsModule,
    ReactiveFormsModule,
    NgApexchartsModule,
  ]
})
export class EntidadModule { }
