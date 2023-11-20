export function getBase64(file: File) {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.readAsDataURL(file);
    reader.onload = () => resolve(reader.result);
    reader.onerror = (error) => reject(error);
  });
}

export function base64toFile(b64Data, filename, contentType) {
  const sliceSize = 512;
  const byteCharacters = atob(b64Data);
  const byteArrays = [];

  for (let offset = 0; offset < byteCharacters.length; offset += sliceSize) {
    const slice = byteCharacters.slice(offset, offset + sliceSize);
    const byteNumbers = new Array(slice.length);

    for (let i = 0; i < slice.length; i++) {
      byteNumbers[i] = slice.charCodeAt(i);
    }
    const byteArray = new Uint8Array(byteNumbers);
    byteArrays.push(byteArray);
  }
  const file = new File(byteArrays, filename, { type: contentType });
  return file;
}

export function base64ToFilePromise(base64, filename, mimeType) {
  return fetch(base64)
    .then((res) => res.arrayBuffer())
    .then((buf) => new File([buf], filename, { type: mimeType }));
}

export function removeDuplicatedItems(array: any[]) {
  const nivelesUniq = array.reduce(function (a, b) {
    if (a.indexOf(b) < 0) a.push(b);
    return a;
  }, []);
  return nivelesUniq;
}

export function validateFile(event): string {

  if (event.target.files[0].size / (1024 * 1024) > 1) {
    return 'El archivo excede el tamaño de 1MB';
  } else {
    const extension = event.target.files[0].name.split('.')[
      event.target.files[0].name.split('.').length - 1
    ];
    const extensionesPermitidas = ['pdf', 'jpg', 'png', 'jpeg'];
    if (extensionesPermitidas.includes(extension.toLowerCase())) {
      return 'ok';

    } else {
      return 'Solo se permiten archivos PDF, JPG, JPEG y PNG';
    }
  }
}

export function validateFileExcel(event): string {

  if (event.target.files[0].size / (1024 * 1024) > 1) {
    return 'El archivo excede el tamaño de 1MB';
  } else {
    const extension = event.target.files[0].name.split('.')[
      event.target.files[0].name.split('.').length - 1
    ];
    const extensionesPermitidas = ['xlsx'];
    if (extensionesPermitidas.includes(extension.toLowerCase())) {
      return 'ok';

    } else {
      return 'Solo se permiten archivos xlsx';
    }
  }
}