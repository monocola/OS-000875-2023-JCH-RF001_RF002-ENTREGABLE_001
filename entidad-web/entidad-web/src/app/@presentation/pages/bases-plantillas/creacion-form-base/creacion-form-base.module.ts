import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { CreacionFormBaseRoutingModule } from './creacion-form-base-routing.module';
import { CreacionFormBaseComponent } from './creacion-form-base.component';
import { LegalComponent } from './legal/legal.component';
import { BonificacionComponent } from './bonificacion/bonificacion.component';
import { AngularResizedEventModule } from 'angular-resize-event';
import { ComponentsBasePlantillasModule } from '../components/components-bases.plantillas.module';
import { MatDividerModule } from '@angular/material/divider';
import { NbButtonModule, NbIconModule } from '@nebular/theme';
import { MatButtonModule } from '@angular/material/button';
import { CommonComponentsModule } from 'src/app/@presentation/@common-components/common-components.module';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { QuillModule } from 'ngx-quill';
import { MatIconModule } from '@angular/material/icon';
import { CKEditorModule } from 'ng2-ckeditor';

@NgModule({
  declarations: [
    CreacionFormBaseComponent,
    LegalComponent,
    BonificacionComponent,
  ],
  imports: [
    CommonModule,
    CKEditorModule,
    FormsModule,
    CreacionFormBaseRoutingModule,
    AngularResizedEventModule,
    ComponentsBasePlantillasModule,
    MatDividerModule,
    NbIconModule,
    MatButtonModule,
    CommonComponentsModule,
    ReactiveFormsModule,
    MatIconModule,
    NbButtonModule,
    QuillModule.forRoot({
      modules: {
        syntax: false,
        toolbar: [
          ['bold', 'italic', 'underline'],
          [{ list: 'ordered' }, { list: 'bullet' }],
          [{ header: [1, 2, 3, 4, 5, 6, false] }],
          [{ align: [] }],
        ],
      },
    }),
  ],
})
export class CreacionFormBaseModule {}
