import { Location } from '@angular/common';
import { AfterViewChecked, Component, OnInit, ViewChild } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { BasesPlantillasRepository } from 'src/app/@domain/repository/bases-plantillas.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { CreacionFormBaseService } from '../creacion-form-base.service';
import { CKEditorComponent } from 'ng2-ckeditor';

@Component({
  selector: 'serv-talento-legal',
  templateUrl: './legal.component.html',
  styleUrls: ['./legal.component.scss'],
})
export class LegalComponent implements OnInit, AfterViewChecked {
  ckeditorContent: string = '';
  @ViewChild(CKEditorComponent) ckEditor: CKEditorComponent;
  bodySize = '62.5rem';
  textSize = 0;
  editorStyle = {
    height: 'calc(225px)',
  };
  baseLegalForm: FormGroup;

  constructor(
    private location: Location,
    public helperService: CreacionFormBaseService,
    private basesPlantillasRepository: BasesPlantillasRepository,
    private toastService: ToastService,
    public router: Router,
    public fb: FormBuilder
  ) {}

  ngOnInit(): void {
    this.initializeForm();
    this.helperService.tipoInforme = JSON.parse(
      sessionStorage.getItem('tipoInforme')
    );
    if (this.helperService.dataToEdit) {
      this.setDataLegal(this.helperService.dataToEdit);
      this.ckeditorContent = this.helperService.dataToEdit.contenido;
      this.helperService.informeIdToEdit = this.helperService.dataToEdit.informeDetalleId;
    }
  }

  setDataLegal(values) {
    this.baseLegalForm.patchValue({
      nombreInforme: values.titulo,
    });
  }

  get f() {
    return this.baseLegalForm.controls;
  }

  onResized(e) {
    this.bodySize = e.newWidth + 'px';
  }

  save() {
    this.baseLegalForm.markAllAsTouched();
    if (this.baseLegalForm.invalid) return;
    if (this.ckeditorContent.length === 0) {
      this.toastService.showToast(
        'El contenido del informe es obligatorio.',
        'warning'
      );
      return;
    }
    const body = this.baseLegalForm.getRawValue();
    body.contenidoInforme = this.ckeditorContent;
    body.tipoInforme = this.helperService.tipoInforme?.descripcion || null;
    body.tipoInformeId = this.helperService.tipoInforme?.maeDetalleId || null;
    this.basesPlantillasRepository
      .saveOrUpdatePlantillaBase(body, this.helperService.informeIdToEdit)
      .subscribe((res) => {
        this.helperService.informeIdToEdit
          ? this.toastService.showToast(
              'Se guardaron los cambios exitosamente',
              'primary'
            )
          : this.toastService.showToast(
              'El informe ha sido creado exitosamente',
              'success'
            );
        this.helperService.initializeValues();
        this.router.navigateByUrl('pages/gestionplantillas');
      });
  }

  goBack() {
    this.location.back();
  }

  getValueLength(e) {
    this.textSize = e.replace(/<[^>]*>/g, '').replace('&nbsp;', '').length;
  }

  initializeForm() {
    this.baseLegalForm = this.fb.group({
      nombreInforme: ['', Validators.required],
    });
  }

  ngAfterViewChecked() {
    let editor = this.ckEditor.instance;
    editor.config.height = '300';

    editor.config.toolbarGroups = [
      { name: 'document', groups: ['mode', 'document', 'doctools'] },
      { name: 'clipboard', groups: ['clipboard', 'undo'] },
      {
        name: 'editing',
        groups: ['find', 'selection', 'spellchecker', 'editing'],
      },
      { name: 'forms', groups: ['forms'] },
      '/',
      { name: 'basicstyles', groups: ['basicstyles', 'cleanup'] },
      {
        name: 'paragraph',
        groups: ['list', 'indent', 'blocks', 'align', 'bidi', 'paragraph'],
      },
      { name: 'links', groups: ['links'] },
      { name: 'insert', groups: ['insert'] },
      { name: 'styles', groups: ['styles'] },
      { name: 'colors', groups: ['colors'] },
      { name: 'tools', groups: ['tools'] },
      { name: 'others', groups: ['others'] },
      { name: 'about', groups: ['about'] },
    ];

    editor.config.removeButtons =
      'Source,Save,NewPage,ExportPdf,Preview,Print,Templates,Cut,Copy,Paste,PasteText,PasteFromWord,Find,Replace,Image,Flash,HorizontalRule,Smiley,SpecialChar,PageBreak,Iframe,Anchor,Language,BidiRtl,BidiLtr,CreateDiv,Indent,Outdent,Subscript,Superscript,Strike,About,Maximize,ShowBlocks,SelectAll,Scayt,HiddenField,ImageButton,Button,Select,Textarea,TextField,Radio,Checkbox,Form,Undo,Redo,Blockquote,CopyFormatting,RemoveFormat,Unlink';
  }
}
