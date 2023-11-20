import { Component, Inject, OnInit } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import * as FileSaver from 'file-saver';

@Component({
  selector: 'serv-talento-file-visualizer',
  templateUrl: './file-visualizer.component.html',
  styleUrls: ['./file-visualizer.component.scss'],
})
export class FileVisualizerComponent implements OnInit {
  base64String = '';
  filename = 'Nombre de archivo';
  extensionFile = '';

  constructor(
    @Inject(MAT_DIALOG_DATA) public data: DataModel,
    private dialogRef: MatDialogRef<FileVisualizerComponent>
  ) {}

  ngOnInit(): void {
    this.filename = this.data.filename;
    this.extensionFile = this.data.extension.toLowerCase();
    this.setString();
  }

  setString() {
    if (this.extensionFile === 'pdf') {
      this.base64String =
        'data:application/pdf;base64,' + this.data.base64String;
    } else {
      this.base64String =
        'data:image/' +
        this.extensionFile +
        ';base64,' +
        this.data.base64String;
    }
  }

  downloadFile() {
    FileSaver.saveAs(this.base64String, this.filename);
  }

  onNoClick() {
    this.dialogRef.close();
  }
}

export interface DataModel {
  base64String: string;
  filename: string;
  extension: string;
}
